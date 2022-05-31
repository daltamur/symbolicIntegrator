package grammarClasses

case class TE(l: T, operation: Char) extends S {
  override var integrationVal: String = _

  override def eval(): Unit = {
    print(operation)
    l.eval()
  }

  override def compute(): Unit = ???

  override def getIntegrationVal(): String = {
    return integrationVal
  }

}
