package grammarClasses

case class E(l: T, r: Option[Either[E2, E3]]) extends S {

  override var integrationVal: String = _

  override def eval(): Unit = {
    /*
    l.eval()
    r match {
      case Some(Left(r)) =>
        r.eval()
      case Some(Right(r)) => r.eval()
      case None => print("")
    }
     */
  }

  override def compute(): Unit = {
    l.compute()
    r match {
      case Some(Left(r)) =>
        r.fork()
        r.join()
        integrationVal = l.getIntegrationVal + "+" + r.getIntegrationVal
      case Some(Right(r)) =>
        r.fork()
        r.join()
        integrationVal = l.getIntegrationVal + "-" + r.getIntegrationVal
      case None => integrationVal = l.getIntegrationVal
    }


  }

  override def getIntegrationVal(): String = {
    return integrationVal
  }

  override def getString: String = {
    r match {
      case None =>
        l.getString
      case _: Option[Either[E2, E3]]=>
        val rVal = r.get
        rVal match {
          case Left(rVal) =>
            l.getString + '+' + rVal.getString
          case Right(rVal) =>
            l.getString+ '-' + rVal.getString
        }
    }
  }
}
