package grammarClasses

case class Var(n: String) extends F {
  override var integrationVal: String = _

  override def eval(): Unit = {
    print(n)
  }

  def getVar: String = {
    return n
  }

  override def compute(): Unit = {
    integrationVal = "(1/2)" + n + "^2"
  }

  override def getIntegrationVal(): String = {
    return integrationVal
  }

  override def getParamVal: Either[String, Double] = {
    Left(n)
  }

  override def runCompute(): Unit = {
    compute()
  }

  override def getString(): String = return n
}
