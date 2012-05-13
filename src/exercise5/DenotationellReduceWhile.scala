package exercise5

import exericse4.ReduceWhile
import exercise3._

object DenotationellReduceWhile {
  type S = ReduceWhile.S
  type E = ReduceWhile.E
  type A = ReduceWhile.A
  type State = ReduceWhile.State
  val store0 = ReduceWhile.store0

  def booleanTerm(bt: BooleanTerm)(state: State): Option[Tuple2[TruthValue, State]] = {
    null
  }


  def term(t: Term)(state: State): Option[Tuple2[Number, State]] = {
    null
  }

  def command(com: Command)(state: State): Option[State] = {
    com match {
      case Skip => Some(state)
      case Assign(Identifier(id), t) =>
        val evaluatedTerm = term(t)(state)
        evaluatedTerm match {
          case Some((Number(n), (s, e, a))) =>
            Some(s + (id -> n),e,a)
          case None => None
        }
    }
  }

  def program(node: Command)(input: E): Option[A] = {
    val result = command(node)((store0,input, Nil))
    result match {
      case Some((_, _, a)) => Some(a.reverse)
      case None => None
    }
  }

  def eval(prog: Command, input: E) = {
    val output = program(prog)(input)
    output match {
      case Some(result) => println(result)
      case None => throw new RuntimeException("Can not evaluate this program or with this input")
    }
  }

}
