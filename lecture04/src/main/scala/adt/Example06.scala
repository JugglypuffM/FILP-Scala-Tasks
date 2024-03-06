package adt

import scala.annotation.tailrec

object Example06 extends App {
  sealed trait MakeGroupsResult
  object MakeGroupsResult {
    case object NoParticipants                  extends MakeGroupsResult
    case class SingleParticipant(login: String) extends MakeGroupsResult
    case class Success(groups: List[Group])     extends MakeGroupsResult
  }

  sealed trait Group
  object Group {
    case class TwoPerson(first: String, second: String)                  extends Group
    case class ThirdPerson(first: String, second: String, third: String) extends Group
  }

  def makeGroups(participants: List[String]): MakeGroupsResult = {
    @tailrec
    def takeGroups(first: String, second: String, restParticipants: List[String], acc: List[Group]): List[Group] =
      restParticipants match {
        case Nil           => Group.TwoPerson(first, second) :: acc
        case single :: Nil => Group.ThirdPerson(first, second, single) :: acc
        case nextFirst :: nextSecond :: rest =>
          takeGroups(nextFirst, nextSecond, rest, Group.TwoPerson(first, second) :: acc)
      }

    participants match {
      case Nil                     => MakeGroupsResult.NoParticipants
      case single :: Nil           => MakeGroupsResult.SingleParticipant(single)
      case first :: second :: tail => MakeGroupsResult.Success(takeGroups(first, second, tail, Nil))
    }
  }

  println(makeGroups(Nil))

  println(makeGroups(List("ilya")))

  println(makeGroups(List("ilya", "ivan", "igor")))

  println(makeGroups(List("ilya", "ivan", "igor", "maria")))
}
