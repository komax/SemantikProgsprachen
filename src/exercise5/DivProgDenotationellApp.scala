package exercise5

import exercise3.DivProgBuilder


object DivProgDenotationellApp {
  def main(args: Array[String]) {
    val divProg = DivProgBuilder.progWithRead
    DenotationellReduceWhile.eval(divProg, List(7,5))
  }

}
