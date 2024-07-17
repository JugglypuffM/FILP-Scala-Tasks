//package lecture10
//
//import cats.effect.implicits.{genSpawnOps, monadCancelOps_}
//import cats.effect.kernel.{Fiber, GenConcurrent, Outcome, Temporal}
//import cats.effect.std.Mutex
//import cats.effect.unsafe.implicits.global
//import cats.effect.{Concurrent, Deferred, IO, Ref, Resource}
//import cats.syntax.all._
//
//object ex19_race extends App {
// class ConcurrentGet[F[_]: Concurrent] {
//   override def racePair[A, B](fa: F[A], fb: F[B])
//   : F[Either[(Outcome[F, Throwable, A], Fiber[F, Throwable, B]), (Fiber[F, Throwable, A], Outcome[F, Throwable, B])]] = {
//     implicit val F: GenConcurrent[F, Throwable] = Concurrent[F]
//
//     uncancelable { poll =>
//       for {
//         result <-
//           deferred[Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]]
//
//         fibA <- start(guaranteeCase(fa)(oc => result.complete(Left(oc)).void))
//         fibB <- start(guaranteeCase(fb)(oc => result.complete(Right(oc)).void))
//
//         back <- onCancel(
//           poll(result.get),
//           for {
//             canA <- start(fibA.cancel)
//             canB <- start(fibB.cancel)
//
//             _ <- canA.join
//             _ <- canB.join
//           } yield ())
//       } yield back match {
//         case Left(oc) => Left((oc, fibB))
//         case Right(oc) => Right((fibA, oc))
//       }
//     }
//   }
// }
////    for {
////      deferred <- Deferred[F, Either[Throwable, A]]
////
////      fibra1 <- f1
////        .flatTap(a => deferred.complete(Right(a)))
////        .handleErrorWith(e => deferred.complete(Left(e)) >> e.raiseError)
////        .start
////
////      fibra2 <- f2
////        .flatTap(a => deferred.complete(Right(a)))
////        .handleErrorWith(e => deferred.complete(Left(e)) >> e.raiseError)
////        .start
////
////      result <- deferred.get // blocking
////
////      _ <- fibra1.cancel
////      _ <- fibra2.cancel
////    } yield result match {
////      case Left(value) => Temporal[F].raiseError(value)
////      case Right(value) => Temporal[F].pure(value)
////    }
//}
