package grammarClasses

case class EP(l: T, r: Option[Either[E2, E3]]) extends F {

  override var integrationVal: String = _

  override def eval(): Unit = {
    //print("<start parenthesis>")
    print('(')
    l.eval()
    r match {
      case Some(Left(r)) => r.eval()
      case Some(Right(r)) => r.eval()
      case None => print("")
    }
    print(')')
    //print("<end parenthesis>")

  }

  override def compute(): Unit = ???

  override def getIntegrationVal(): String = {
    return integrationVal
  }

  override def getParamVal: Either[String, Double] = ???

  override def runCompute(): Unit = {
    compute()
  }

  override def getString: String = {
    r match {
      case None =>
        '('+l.getString+')'
      case _: Option[Either[E2, E3]]=>
        val rVal = r.get
        rVal match {
          case Left(rVal) =>
            '('+l.getString + '+' + rVal.getString+')'
          case Right(rVal) =>
            '('+l.getString+ '-' + rVal.getString+')'
        }
    }
  }
}
