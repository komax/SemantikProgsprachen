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

  def binOpEval(op: ArithmeticOp, n1: Int, n2: Int)(state: State): Option[Tuple2[Number,State]] = {
    op match {
      case ArithmeticOp.Plus => Some(Number(n1 + n2), state)
      case ArithmeticOp.Minus => Some((Number(n1 - n2), state))
      case ArithmeticOp.Times => Some((Number(n1 * n2), state))
      case ArithmeticOp.Div =>
        if (n2 == 0) throw new RuntimeException("Division with zero is not allowed")
        Some((Number(n1 / n2), state))
      case ArithmeticOp.Mod => Some((Number(n1 % n2), state))
      case _ => None
    }
  }


  def term(t: Term)(state: State): Option[Tuple2[Number, State]] = {
    val (s,e,a) = state
    t match {
      case Number(n) => Some(Number(n), state)
      case Identifier(id) =>
        if (s.contains(id))
          Some(Number(s(id)), state)
        else
          None
      case BinOp(op, leftTerm, rightTerm) =>
        term(leftTerm)(state) match {
          case Some((Number(n1), state1)) =>
            term(rightTerm)(state1) match {
              case Some((Number(n2), state2)) => binOpEval(op, n1, n2)(state2)
              case None => None
            }
          case None => None
        }
      case Read =>
        if (e.isEmpty)
          None
        else {
          val n :: es = e
          Some(Number(n), (s,es, a))
        }
    }
  }

  def command(com: Command)(state: State): Option[State] = {
    com match {
      case Skip => Some(state)
      case Assign(Identifier(id), t) =>
          term(t)(state) match {
          case Some((Number(n), (s, e, a))) =>
            Some(s + (id -> n),e,a)
          case None => None
        }
      case CommandSeq(firstCom :: rest) =>
          command(firstCom)(state) match {
          case Some(newState) => command(CommandSeq(rest))(newState)
          case None => None
        }
      case CommandSeq(Nil) => Some(state)
      case If(condition, thenCom, elseCom) =>
        booleanTerm(condition)(state) match {
          case Some((TruthValue(b), newState)) =>
            if (b)
              command(thenCom)(newState)
            else
              command(elseCom)(newState)
          case None => None
        }
      case While(condition, body) =>
        booleanTerm(condition)(state) match {
          case Some((TruthValue(b), newState)) =>
            if (b)
              command(CommandSeq(List(body, While(condition,body))))(newState)
            else
              Some(newState)
          case None => None
        }
      case OutputTerm(t) =>
        term(t)(state) match {
          case Some((Number(n), (s, e, a))) =>
            Some(s,e,n :: a)
          case None => None
        }
      case OutputBTerm(bt) =>
        booleanTerm(bt)(state) match {
          case Some((TruthValue(b), (s, e, a))) =>
            val n = if (b) 1 else 0
            Some(s,e, n :: a)
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
