package grammarClasses

case class E2(l: E) extends S {
  override var integrationVal: String = _


  override def eval(): Unit = {
    print('+')
    l.eval()
  }

  override def compute(): Unit = {
    l.compute()
    integrationVal = l.getIntegrationVal()
  }

  override def getIntegrationVal(): String = {
    return integrationVal
  }

}
