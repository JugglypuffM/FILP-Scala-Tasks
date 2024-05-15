package registry.adapter.kafka

import cats.effect._
import cats.effect.syntax.all._
import cats.syntax.all._
import fs2.kafka._
import io.circe._
import io.circe.parser._
import registry.domain.model._
import registry.domain.service.{EventDispatcherAlg, EventHandlerAlg}

import scala.concurrent.duration._

object EventObserverAlg {
  def build[F[_]: Async](host: String, port: Int, topicName: String, group: String): EventDispatcherAlg[F] =
    new EventDispatcherAlgInterpreter[F](host, port, topicName, group: String)

  private class EventDispatcherAlgInterpreter[F[_]: Async](
      host: String,
      port: Int,
      topicName: String,
      group: String
  ) extends EventDispatcherAlg[F] {

    //Тут вручную определяется парсер json'а, прилетающего в событии из топика кафки
    implicit val decoder: Decoder[ApprovalEvent] = (c: HCursor) =>
      for {
        rId <- c.downField("requestId").as[String]
        res <- c.downField("result").as[String].flatMap {
          case "trust"    => Right(ApprovalEvent.Result.Trust)
          case "mistrust" => Right(ApprovalEvent.Result.Mistrust)
          case x          => Left(DecodingFailure(s"Incorrect value: $x", Nil))
        }
      } yield ApprovalEvent(Application.Id(rId), res)

    //Для парсинга сообщений из кафки используется тип Deserializer.
    //Конкретно тут, десериалайзер для ApprovalEvent выводится из, поставляемого библиотекой,
    //десериалайзера для простых строк
    implicit val deserializer: Deserializer[F, ApprovalEvent] = Deserializer[F, String].flatMap { raw =>
      decode[ApprovalEvent](raw) match {
        case Right(event) => Deserializer.const(event)
        case Left(err)    => Deserializer.fail(err)
      }
    }

    // Тут определяются настройки подключения к кластеру kafka
    private val consumerSettings =
      ConsumerSettings[F, Option[String], ApprovalEvent]
        .withAutoOffsetReset(AutoOffsetReset.Latest)
        .withBootstrapServers(s"$host:$port")
        .withGroupId(group)

    // Тут инициализируется потребитель событий и происходит подписка на
    // события из топика trustworthiness
    // Для обработки событий используется обработчик EventHandlerAlg
    override def start(eha: EventHandlerAlg[F]): F[Unit] = {
      KafkaConsumer
        .stream(consumerSettings)
        .subscribeTo(topicName)
        .records
        .mapAsync(25) { commitable =>
          eha.handle(commitable.record.value).as(commitable.offset)
        }
        .through(commitBatchWithin(500, 15.seconds))
        .compile
        .drain
        .start
        .as(())
    }
  }
}
