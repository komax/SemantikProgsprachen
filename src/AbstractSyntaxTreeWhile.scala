sealed abstract class WhileASTNode

abstract class Command extends WhileASTNode
case object Skip extends Command
case class Assign(lvalue: Identifier, rvalue: Term) extends Command
case class CommandSeq(seq: List[Command]) extends Command
case class If(condition: BooleanTerm, thenCommand: Command, elseCommand: Command) extends Command
case class While(condition: BooleanTerm, body: Command) extends Command
case class OutputTerm(term: Term) extends Command
case class OutputBTerm(bTerm: BooleanTerm) extends Command

abstract class Term extends WhileASTNode
case class Number(num: Int) extends Term
case class Identifier(id: String) extends Term
case object Read extends Term
case class BinOp(op: ArithmeticOp, left: Term, right: Term) extends Term

abstract class BooleanTerm extends WhileASTNode
case class TruthValue(value: Boolean) extends BooleanTerm
case object BRead extends BooleanTerm
case class Not(arg: BooleanTerm) extends BooleanTerm
case class BinBooleanOp(op: BooleanOp, left: Term, right: Term) extends BooleanTerm
