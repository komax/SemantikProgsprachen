package exericse4

import exercise3._


object ReduceWhile {
  type T = WhileASTNode
  type S = WSKEAMachine.S
  type E = WSKEAMachine.E
  type A = WSKEAMachine.A
  type State = Tuple3[S,E,A]
  type ReduceTerm = Tuple2[T,State]
  val store0: S = WSKEAMachine.store0
  type WSKEA = WSKEAMachine.WSKEA

  // Aufgabe 1
  def initProgram(prog: T, input: E): ReduceTerm =
    (prog,(store0, input, Nil))

  def semanticFunc(rTerm: ReduceTerm): ReduceTerm = {
    val (term, _) = rTerm
    val wskea = initWSKEA(rTerm)
    term match {
      case Skip => rTerm
      case t: Term => semanticFunc(deltaKArithmetic(wskea))
      case b: BooleanTerm => semanticFunc(deltaKBooleanTerm(wskea))
      case c: Command => semanticFunc(deltaKCommand(wskea))
      case _ => throw new RuntimeException("can not apply semantic reduce function")
    }
  }

  def initWSKEA(rTerm: ReduceTerm): WSKEA = {
    val (t,(s,e,a)) = rTerm
    (Nil, s, t :: Nil, e, a)
  }

  def deltaKArithmetic(wskea: WSKEA): ReduceTerm = {
    val (w,s,_,e,a) = WSKEAMachine.delta(wskea)
    val Number(n) :: ws = w
    (Number(n), (s,e,a))
  }

  def deltaKBooleanTerm(wskea: WSKEA): ReduceTerm = {
    val (w,s,_,e,a) = WSKEAMachine.delta(wskea)
    val TruthValue(b) :: ws = w
    (TruthValue(b), (s,e,a))
  }

  def deltaKCommand(wskea: WSKEA): ReduceTerm = {
    val (_,s,_,e,a) = WSKEAMachine.delta(wskea)
    (Skip, (s,e,a))
  }

  // Aufgabe 2
  def eval(prog: T, input: E) = {
    val startTuple = initProgram(prog, input)
    val result: Option[ReduceTerm] = reduce(startTuple)
    result match {
      case Some((Skip, (s,e,a))) => a.reverse
      case Some((t, (s,e,a))) => throw new RuntimeException("can not reduce this term")
      case None => throw new RuntimeException("undefined state")
    }
  }

  def reduce(rTerm: ReduceTerm): Option[ReduceTerm] = {
    null
  }

  def reduceStep(rTerm: ReduceTerm): Option[ReduceTerm] = {
    val (t, oldState) = rTerm
    val (s,e,a) =   oldState
    t match {
      case Identifier(id) =>
        if (s.contains(id))
          Some((Number(s(id)), oldState))
        else
          None
      case BinOp(op,left, right) =>
        val leftReduce = reduce(left, oldState)
        leftReduce match {
          case Some((Number(num), newState)) => Some((BinOp(op, Number(num), right), newState))
          case None => None
        }
      case BinOp(op, Number(num), right) =>
        val rightReduce = reduce(right, oldState)
        rightReduce match {
          case Some((Number(mun), newState)) => Some((BinOp(op, Number(num), Number(mun)), newState))
          case None => None
        }
      case BinOp(op, Number(num), Number(mun)) =>
        op match {
          case ArithmeticOp.Plus => Some((Number(num + mun), oldState))
          case ArithmeticOp.Minus => Some((Number(num - mun), oldState))
          case ArithmeticOp.Times => Some((Number(num * mun), oldState))
          case ArithmeticOp.Div =>
            if (mun == 0) throw new RuntimeException("Division with zero is not allowed")
            Some((Number(num / mun), oldState))
          case ArithmeticOp.Mod => Some((Number(num % mun), oldState))
          case _ => None
        }
    }
  }

}
