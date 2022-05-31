package grammarClasses

case class Const(v: Double) extends F {
  //def eval(env: Main.Environment): Int = v
  override var integrationVal: String = _

  override def eval(): Unit = {
    print(v)
  }

  override def compute(): Unit = {
    integrationVal = v + "x"
  }

  def getConstVal: Double = {
    return v
  }

  override def getIntegrationVal(): String = {
    return integrationVal
  }

  override def getParamVal: Either[String, Double] = {
    Right(v)
  }

  override def runCompute(): Unit = {
    compute()
  }
}
