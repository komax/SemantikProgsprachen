object DivProgBuilder {
  val x = Identifier("x")
  val y = Identifier("y")
  val g = Identifier("g")
  val zero = Number(0)
  val one = Number(1)

  def buildProg: Command = {
    val cond = BinBooleanOp(BooleanOp.GEq, x, y)
    val body = CommandSeq(
      Assign(g, BinOp(ArithmeticOp.Plus, g, one))
      :: Assign(x, BinOp(ArithmeticOp.Minus, x, y))
      :: Nil
    )
    val whilePart = While(cond, body)
    val output = OutputTerm(g)
    CommandSeq(
      whilePart
      :: output
      :: Nil
    )
  }

  def progWithRead: Command = {
    CommandSeq(
    Assign(x, Read)
    :: Assign(y, Read)
    :: Assign(g, zero)
    :: buildProg
    :: OutputTerm(x)
    :: Nil
    )
  }

}
