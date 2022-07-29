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

  def getNestedUVals(currentNode: F): Unit = {
    currentNode match {
      case _: EP =>
        getPossibleUValues(currentNode.asInstanceOf[EP].l)
        checkEExtension(currentNode.asInstanceOf[EP].r)
        println(currentNode.getString())

      case _ => println("No U's Here")
    }
  }

   def getPossibleUValues(currentNode: T): Unit = {
     //Since we know a U is almost certainly a nested expression, we will first look for the nested expressions
     currentNode.l match {
       case _: EP =>
         getNestedUVals(currentNode.l)
         println(currentNode.getString)

       case _: FExp =>
         getNestedUVals(currentNode.l.asInstanceOf[FExp].l)
         getNestedUVals(currentNode.l.asInstanceOf[FExp].r)

       case _ =>
         checkTExtension(currentNode.r)

     }

  }

  def checkEExtension(currentNode: Option[Either[E2, E3]]): Unit = {
    currentNode match {
      case Some(value) =>
        value match {
          case Left(value)  => getPossibleUValues(value.l.l)
          case Right(value) => getPossibleUValues(value.l.l)
        }

      case _ => println("Nested expression finished")
    }
  }

  def checkTExtension(currentNode: Option[TE]): Unit = {
    currentNode match {
      case Some(value) => getPossibleUValues(value.l)
      case _ => println("No further U's here")
    }
  }


  override def compute(): Unit = {
    //grammarClasses.F->'('grammarClasses.E')'|var|const|grammarClasses.FExp|Sin(grammarClasses.E)
    //When we are computing an grammarClasses.F, we either have an grammarClasses.EP (an grammarClasses.E expression nested in parentheses), a simple variable letter,
    //some constant value, or an exponent. For now, we are going to worry about the constant values
    getPossibleUValues(this)
    r match {
      //there is some multiplication or division happening if we have a TE
      case Some(r) =>
        r.l.r match {
          //if the TE doesn't have a TE itself, then we know we likely have a simple exponent rule
          //for now, this is the one we are going to deal with.
          case None =>
            l match {
              case _: Const =>
                r.l.l match {
                  case _: Var =>
                    r.operation match {
                      //we are just going to deal with multiplication for now, a little later on we'll handle division
                      case '*' => exponentRule()
                    }

                  case _: FExp =>
                    r.operation match {
                      //we are just going to deal with multiplication for now, a little later on we'll handle division
                      case '*' => exponentRule()
                    }
                }
              case _: Var =>
                r.l.l match {
                  case _: Const =>
                    r.operation match {
                      //note that this will throw up an error since I don't have it set for someone to write sometihng like
                      //x*5, but it's a simple addition I just can't be bothered to include right now.
                      case '*' => exponentRule()
                    }
                }
            }
        }

      //There is no TE
      case None =>
        l.runCompute()
        integrationVal = l.getIntegrationVal
    }
  }

  override def getString: String = {
    r match {
      case None =>
        l.getString()

      case _:Option[TE] =>
        l.getString+ r.get.getString
    }
  }
}
