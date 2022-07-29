package grammarClasses

abstract class F extends S{
  def getParamVal: Either[String, Double]
  def runCompute()
  def getString(): String
}
