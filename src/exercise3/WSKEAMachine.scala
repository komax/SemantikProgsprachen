package exercise3

object WSKEAMachine {
  type W = List[Any]
  type S = Map[String, Int]
  type K = List[Any]
  type E = List[Int]
  type A = List[Int]
  type WSKEA = Tuple5[W, S, K, E, A]
  val store0: S = Map.empty[String, Int]

  sealed abstract class KSymbol
  case object NotK extends KSymbol
  case class AssignK(id: Identifier) extends KSymbol
  case class IfK(thenCom: Command, elseCom: Command) extends KSymbol
  case class WhileK(cond: BooleanTerm, body: Command) extends KSymbol
  abstract class OutputK extends KSymbol
  case object OutputIntK extends OutputK
  case object OutputBoolK extends OutputK

  def anfang(program: Command, input: E): WSKEA = {
    (Nil, store0, program :: Nil, input, Nil)
  }

  var steps = 0

  def delta(wskea: WSKEA): WSKEA = {
    var wskeaBefore = wskea
    steps = 1
    var wskeaAfter = deltaStep(wskeaBefore)
    while (wskeaBefore != wskeaAfter) {
      wskeaBefore = wskeaAfter
      wskeaAfter = deltaStep(wskeaBefore)
      steps += 1
    }
    wskeaBefore
  }

  def deltaStep(wskea: WSKEA): WSKEA = {
    wskea match {
      case (w, s, k :: ks, e, a) =>
        k match {
          // part for terms
          case Number(num) => (Number(num) :: w, s, ks, e, a)
          case Identifier(id) => (Number(s(id)) :: w, s, ks, e, a)
          case BinOp(op, left, right) => (w, s, left :: right :: op :: ks, e, a)
          case Read =>
            if (e.isEmpty) throw new RuntimeException("Can not apply read: Input e is empty")
            val n :: es = e
            (Number(n) :: w, s, ks, es, a)
          case op: ArithmeticOp =>
            val Number(n2) :: Number(n1) :: ws = w
            op match {
              case ArithmeticOp.Plus => (Number(n1 + n2) :: ws, s, ks, e, a)
              case ArithmeticOp.Minus => (Number(n1 - n2) :: ws, s, ks, e, a)
              case ArithmeticOp.Times => (Number(n1 * n2) :: ws, s, ks, e, a)
              case ArithmeticOp.Div =>
                if (n2 == 0) throw new RuntimeException("Division with zero is not allowed")
                (Number(n1 / n2) :: ws, s, ks, e, a)
              case ArithmeticOp.Mod => (Number(n1 % n2) :: ws, s, ks, e, a)
              case _ => throw new RuntimeException("unknown binary operation")
            }

          // part for boolean terms
          case TruthValue(value) => (TruthValue(value) :: w, s, ks, e, a)
          case BRead =>
            if (e.isEmpty) throw new RuntimeException("Can not apply read: Input e is empty")
            val n :: es = e
            val b = if (n != 0) true else false
            (TruthValue(b) :: w, s, ks, es, a)
          case Not(arg) => (w, s, arg :: NotK :: ks, e,a)
          case NotK =>
            val TruthValue(value) :: ws = w
            (TruthValue(!value) :: ws, s, ks, e, a)
          case BinBooleanOp(op,left, right) => (w, s, left :: right :: op :: ks, e, a)
          case op: BooleanOp =>
            val Number(n2) :: Number(n1) :: ws = w
            op match {
              case BooleanOp.Eq => (TruthValue(n1 == n2) :: ws, s, ks, e, a)
              case BooleanOp.GEq => (TruthValue(n1 >= n2) :: ws, s, ks, e, a)
              case BooleanOp.Greater => (TruthValue(n1 > n2) :: ws, s, ks, e, a)
              case BooleanOp.LEq => (TruthValue(n1 <= n2) :: ws, s, ks, e, a)
              case BooleanOp.Less => (TruthValue(n1 < n2) :: ws, s, ks, e, a)
              case BooleanOp.NEq => (TruthValue(n1 != n2) :: ws, s, ks, e, a)
              case _ => throw new RuntimeException("unknown boolean binary operation")
            }

          // part for commands
          case Skip => (w, s, ks, e, a)
          case Assign(lvalue, rvalue) => (w, s, rvalue :: AssignK(lvalue) :: ks, e, a)
          case AssignK(Identifier(id)) =>
            val Number(n) :: ws = w
            (ws, s + (id -> n), ks, e, a)
          case CommandSeq(seq) => (w,s, seq ::: ks, e, a)
          case If(condition, thenCommand, elseCommand) =>
            (w, s, condition :: IfK(thenCommand, elseCommand) :: ks, e, a)
          case IfK(thenCom, elseCom) =>
            val TruthValue(b) :: ws = w
            if (b)
              (ws, s, thenCom :: ks, e, a)
            else
              (ws, s, elseCom :: ks, e, a)
          case While(condition, body) =>
            (w, s, condition :: WhileK(condition, body) :: ks, e, a)
          case WhileK(cond, body) =>
            val TruthValue(b) :: ws = w
            if (b)
              (ws, s, body :: cond :: WhileK(cond, body) :: ks, e, a)
            else
              (ws, s, ks, e, a)
          case OutputTerm(term) => (w,s, term :: OutputIntK :: ks, e, a)
          case OutputIntK =>
            val Number(n) :: ws = w
            (ws, s, ks, e, n :: a)
          case OutputBTerm(bTerm) => (w,s, bTerm :: OutputBoolK :: ks, e, a)
          case OutputBoolK =>
            val TruthValue(b) :: ws = w
            val n = if (b) 1 else 0
            (ws, s, ks, e, n :: a)
          case _ => throw new RuntimeException("can not apply delta step on"+wskea)
        }
      case (w, s, Nil, e, a) => (w,s, Nil, e, a)
    }
  }
}
