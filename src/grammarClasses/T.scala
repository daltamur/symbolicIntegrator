package grammarClasses

case class T(l: F, r: Option[TE]) extends S {
  override var integrationVal: String = _

  override def eval(): Unit = {
    //print("<Start T>")
    l.eval()
    r match {
      //there is some multiplication or division happening if we have a TE
      case Some(r) =>
        r.l.r match {
          //if the TE doesn't have a TE itself, then we know we likely have a simple exponent rule
          //for now, this is the one we are going to deal with.
          case None => r.operation match {
            //we are just going to deal with multiplication for now, a little later on we'll handle division
            case '*' =>
          }
        }

      //There is no TE
      case None => print("")
    }

    //print("<End T>")
  }

  override def getIntegrationVal: String = {
    return integrationVal
  }

  def exponentRule(): Unit = {
    //first, let's get the exponent
    var newExponent = -1
    l match {
      case _: Const =>
        r match {
          //we already know there is a tail if we get this far, but we can only access the tail's variables if we do this pattern matching step
          case Some(r) =>
            r.l.l match {
              case _: Const =>
                //if we have two constants, just multiply them together and throw an x on the end
                l.getParamVal match {
                  case Right(leftValue) =>
                    r.l.l.getParamVal match {
                      case Right(rightValue) =>
                        val multiplicationVal = leftValue * rightValue
                        integrationVal = multiplicationVal + "x"
                    }
                }

              case _: Var =>
                //so we have something like 5*x, so all we're going to do is divide the constant by 2 and slap an x on the end.
                l.getParamVal match {
                  case Right(leftValue) =>
                    val newCoefficient = leftValue / 2.0
                    r.l.l.getParamVal match {
                      case Left(variableLetter) =>
                        integrationVal = newCoefficient + variableLetter + "^2"
                    }
                }

              case _: FExp =>
                //this is for if we have something like 5*x^2. We are going to make sure the exponent is a variable raised to a constant
                //(later on this will be more robust)
                val fexpVal = r.l.l.asInstanceOf[FExp]
                fexpVal.l match {
                  case _: Var =>
                    val base = fexpVal.l.asInstanceOf[Var]
                    fexpVal.r match {
                      case _: Const =>
                        val exponent = fexpVal.r.asInstanceOf[Const]
                        val multiplier = l.asInstanceOf[Const]
                        val newExponent = exponent.v + 1.0
                        val newMultiplier = multiplier.v / newExponent
                        integrationVal = newMultiplier + base.n + "^" + newExponent
                    }
                }
            }
        }
    }

  }

  override def compute(): Unit = {
    //grammarClasses.F->'('grammarClasses.E')'|var|const|grammarClasses.FExp|Sin(grammarClasses.E)
    //When we are computing an grammarClasses.F, we either have an grammarClasses.EP (an grammarClasses.E expression nested in parentheses), a simple variable letter,
    //some constant value, or an exponent. For now, we are going to worry about the constant values
    r match {
      //there is some multiplication or division happening if we have a TE
      case Some(r) =>
        r.l.r match {
          //if the TE doesn't have a TE itself, then we know we likely have a simple exponent rule
          //for now, this is the one we are going to deal with.
          case None =>
            r.operation match {
              //we are just going to deal with multiplication for now, a little later on we'll handle division
              case '*' => exponentRule()
            }
        }

      //There is no TE
      case None =>
        l.runCompute()
        integrationVal = l.getIntegrationVal
    }
  }
}
