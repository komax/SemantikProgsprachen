package exericse4

import exercise3._

object DivProgReduceTester {
  def main(args: Array[String]) {
    val divProg = DivProgBuilder.progWithRead
    val rTerm = ReduceWhile.initProgram(divProg, List(7,5))
    println(rTerm)
    // Aufgabe 1
    val rTermEnde = ReduceWhile.semanticFunc(rTerm)
    println(rTermEnde)

    // Aufgabe 2
    val output = ReduceWhile.eval(divProg, List(7,5))
    println(output)
  }
}
