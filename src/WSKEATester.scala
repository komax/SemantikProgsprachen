object WSKEATester {
  def main(args: Array[String]) {
    val divProg = DivProgBuilder.progWithRead
    val wskeaStart = WSKEAMachine.anfang(divProg, List(7, 5))
    println(wskeaStart)
    val wskeaEnde = WSKEAMachine.delta(wskeaStart)
    println(wskeaEnde)
    println("Anzahl der Schritte: "+WSKEAMachine.steps)
  }
}
