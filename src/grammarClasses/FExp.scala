package grammarClasses

case class FExp(l: F, r: F) extends F {
  override var integrationVal: String = _

  override def eval(): Unit = {
    l.eval()
    print('^')
    r.eval()
  }

  def getBase: F = {
    return l
  }

  def getExponent: F = {
    return r
  }

  override def compute(): Unit = {
    l match {
      //we love pattern matching, folks
      case _: Var =>
        r match {
          //no known integrals for something like x^x, so we will only worry about if we have something like
          //1^x or x^1. Later on, we'll need to add some sort of exception catcher
          case _: Const =>
            val exp = r.getParamVal
            var newCoefficient = ""
            var newExp = 0.0
            r.getParamVal match {
              case Right(doubleVal) =>
                newExp = doubleVal + 1.0
                newCoefficient = "1/" + newExp.toInt
            }
            integrationVal = "(" + newCoefficient + ")" + "x^" + newExp.toInt
        }


      case _: Const => r match {
        //it is gratuitous right now to do it with two constants, so we'll do it if we have x^5 or something like that
        case _: Var =>
          var variableLetter = ""
          var baseValue = 0.0
          r.getParamVal match {
            case Left(variable) => variableLetter = variable
          }
          l.getParamVal match {
            case Right(doubleVal) => baseValue = doubleVal
          }
          integrationVal = "((" + baseValue + "^" + variableLetter + ")/ln(" + baseValue + "))"

      }

      case _ =>
    }


  }

  override def getIntegrationVal(): String = {
    return integrationVal
  }

  //this is a placeholder, we'll never really use this function
  override def getParamVal: Either[String, Double] = {
    Left("NULL")
  }

  override def runCompute(): Unit = {
    compute()
  }

  override def getString(): String = l.getString+'^'+r.getString
}
