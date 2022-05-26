import java.awt.Choice
import java.util
import scala.collection.mutable
import scala.util.control.Breaks.break
import scala.util.matching.Regex
import java.time.LocalDateTime
import java.util.concurrent._
import java.util.concurrent.ForkJoinTask
import javax.management._
import java.lang.management.ManagementFactory
import scala.reflect.internal.ClassfileConstants
import scala.reflect.internal.ClassfileConstants.instanceof

//S->E
//E-> Term Term_Tail
//Term-> factor factor_tail
//this keeps addition and subtraction towards the top of the tree, so they are done last
//Term_Tail->'+'E|'-'E|null
//factor-> '('E')'|var|num
//this puts multiplication and division to the bottom of the tree, so it is done last
//factor_tail-> '*'Term

//Doing it this way isn't really conducive of a tree structure...here's a better grammar:
//Though this is arguably an ambigiuous grammar, it will get our job done!
//things in brackets represent an optional token
// S = Statement
// E = Expression
// T = Terminal
// F = Factor
// var = variable
// const = constant value (some number)
//___
// for the sake of this project, we're not going to worry about accepting decimal numbers (after all, this is for
// symbolic integration)

// Later on, we're going to have it accept exponents and negative numbers though (I was just struck with inspiration on
// how to do this and need to work those into terminal/factor tokens...


//E->T [E2]|T [E3]
//E2-> '+' E
//E3-> '-' E
//T->F [TE]
//TE-> '*' T| '/' T
//F->'('E')'|var|const|FExp|Sin(E)
//Note that there can be a negative sign on each possible F, this muddles things up a bit but it's necessary
//FExp -> F'^'F
//we're gonna use case classes just b/c they include the to-string method from the get-go

abstract class S extends java.util.concurrent.RecursiveAction{
  // some sort of abstract function would go here
  var integrationVal: String
  def eval()
  def getIntegrationVal: String
}

abstract class F extends S{
  def getParamVal: Either[String, Double]
  def runCompute()
}

case class E(l: T, r: Option[Either[E2, E3]]) extends S{

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
        integrationVal = l.getIntegrationVal+"+"+r.getIntegrationVal
      case Some(Right(r)) =>
        r.fork()
        r.join()
        integrationVal = l.getIntegrationVal+"-"+r.getIntegrationVal
      case None => integrationVal = l.getIntegrationVal
    }



  }

  override def getIntegrationVal(): String = {return integrationVal}
}

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

  override def getIntegrationVal(): String = {return integrationVal}

  override def getParamVal: Either[String, Double] = ???

  override def runCompute(): Unit = {compute()}
}

case class E2(l: E) extends S{
  override var integrationVal: String = _


  override def eval(): Unit = {
    print('+')
    l.eval()
  }

  override def compute(): Unit = {
    l.compute()
    integrationVal = l.getIntegrationVal()
  }

  override def getIntegrationVal(): String = {return integrationVal}

}

case class E3(l: E) extends S{

  override var integrationVal: String = _

  override def eval(): Unit = {
    print('-')
    l.eval()
  }

  override def compute(): Unit = {
    l.compute()
    integrationVal = l.getIntegrationVal()
  }

  override def getIntegrationVal(): String = {return integrationVal}
}

case class T(l: F, r: Option[TE]) extends S{
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

  override def getIntegrationVal: String = {return integrationVal}

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
                  val newCoefficient = leftValue/2.0
                  r.l.l.getParamVal match {
                    case Left(variableLetter) =>
                      integrationVal = newCoefficient+variableLetter+"^2"
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
                      val newExponent = exponent.v+1.0
                      val newMultiplier = multiplier.v/newExponent
                      integrationVal = newMultiplier+base.n+"^"+newExponent
                  }
              }
          }
      }
    }

  }

  override def compute(): Unit = {
    //F->'('E')'|var|const|FExp|Sin(E)
    //When we are computing an F, we either have an EP (an E expression nested in parentheses), a simple variable letter,
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

case class TE(l: T, operation: Char) extends S{
  override var integrationVal: String = _
  override def eval(): Unit = {
    print(operation)
    l.eval()
  }

  override def compute(): Unit = ???

  override def getIntegrationVal(): String = {return integrationVal}

}


case class FExp(l: F, r: F) extends F{
  override var integrationVal: String = _
  override def eval(): Unit = {
    l.eval()
    print('^')
    r.eval()
  }

  def getBase: F = {return l}

  def getExponent: F = {return r}

  override def compute(): Unit = {
    l match {
      //we love pattern matching, folks
      case _: Var =>
        r match {
          //no known integrals for something like x^x, so we will only worry about if we have something like
          //1^x or x^1. Later on, we'll need to add some sort of exception catcher
          case _:Const =>
            val exp = r.getParamVal
            var newCoefficient = ""
            var newExp = 0.0
            r.getParamVal match {
              case Right(doubleVal) =>
                newExp = doubleVal + 1.0
                newCoefficient = "1/"+newExp.toInt
            }
            integrationVal = "("+newCoefficient+")"+"x^"+newExp.toInt
        }


      case _: Const =>r match {
        //it is gratuitous right now to do it with two constants, so we'll do it if we have x^5 or something like that
        case _:Var =>
          var variableLetter = ""
          var baseValue = 0.0
          r.getParamVal match {
            case Left(variable) => variableLetter = variable
          }
          l.getParamVal match {
            case Right(doubleVal) => baseValue = doubleVal
          }
          integrationVal = "(("+baseValue+"^"+variableLetter+")/ln("+baseValue+"))"

      }

      case _ =>
    }


  }

  override def getIntegrationVal(): String = {return integrationVal}

  //this is a placeholder, we'll never really use this function
  override def getParamVal: Either[String, Double] = {Left("NULL")}

  override def runCompute(): Unit = {compute()}
}

case class Var(n: String) extends F {
  override var integrationVal: String = _
  override def eval(): Unit = {print(n)}
  def getVar: String = {return n}
  override def compute(): Unit = {integrationVal = "(1/2)"+n+"^2"}
  override def getIntegrationVal(): String = {return integrationVal}

  override def getParamVal: Either[String, Double] = {Left(n)}

  override def runCompute(): Unit = {compute()}
}

case class Const(v: Double) extends F {
  //def eval(env: Main.Environment): Int = v
  override var integrationVal: String = _
  override def eval(): Unit = {print(v)}

  override def compute(): Unit = {integrationVal = v+"x"}

  def getConstVal: Double = {return v}

  override def getIntegrationVal(): String = {return integrationVal}

  override def getParamVal: Either[String, Double] = {Right(v)}

  override def runCompute(): Unit = {compute()}
}


class full_expression_parser(input: String) {
  val constregex: Regex = "^(\\-|(\\d(\\.))?)\\d+(\\.\\d+)?".r
  val varregex: Regex = "^[A-Za-z]+".r

  //this will serve as our incrementer in parsing the expression
  var index = 0

  def parseS(): S = parseE()

  def parseE(): E = E(parseT(false), parseETail())

  def parseETail(): Option[Either[E2, E3]] = {
    //if it is adding, then we have an E2 class
    if (index < input.length && input(index) == '+'){
      //println("index at ETail")
      index+=1; // Advance past +
      //println(index)
      Some(Left(E2(parseE())))
    //if it is subtracting, then we have an E3 class
    }else if(index < input.length && input(index) == '-'){
      //println("Index at ETail")
      index+=1; // Advance past +
      //println(index)
      Some(Right(E3(parseE())))
    }
   //if neither of the above conditions are met, then we have reached the end of this part of the expression
   else None
  }

  def parseT(isExponent: Boolean): T = {
    if (input(index) != '-'){
      T(parseF(), parseTTail())
    }else{
      val negconstregex: Regex = "^\\-[0-9]+(\\.[0-9]+)?".r
      val currStrVal = input.substring(index)
      val constsVal = negconstregex.findAllIn(currStrVal)
      if(constsVal.hasNext){
        T(parseF(), parseTTail())
      }else{
        index+=1
        T(Const(-1.0),(Some(TE(parseT(true), '*'))) )
      }

    }
  }

  def parseTTail(): Option[TE] = {
    //if it is multiplying, then we have an E2 class
    if (index < input.length && input(index) == '*'){
      index+=1; // Advance past *
      Some(TE(parseT(false), '*'))
      //if it is dividing, then we have an E3 class
    }else if(index < input.length && input(index) == '/'){
      index+=1; // Advance past /
      Some(TE(parseT(false), '/'))
    }
    //if neither of the above conditions are met, then we have reached the end of this part of the expression
    else None

  }

  def parseF(): F = {
    // Get the unparsed part of the string.
    val currStr = input.substring(index)
    // Get either the const or var which is there.
    val consts = constregex.findAllIn(currStr)
    //print(consts.hasNext)
    if (consts.hasNext){
      val const: String = consts.next()
      index += const.length()
      //println("index at F")
      //println(const.toInt)
      //println(index)
      Const(const.toDouble)
      if(index <= input.length-1){
        if(input(index) == '^'){
          index+=1
          if(input(index) != '-') {
            FExp(Const(const.toDouble), parseF())
          }else{
            //If we have a negative value put together with the exponent, there's a few things we need to do.
            //first we'll check if this is just a negative number
            //if the character after the current index is a '(', then we really have (-1*EP)
            //if the character after the current index is a variable letter, then we really have (-1*var)
            val negconstregex: Regex = "^\\-[0-9]+(\\.[0-9]+)?".r
            val currStrVal = input.substring(index)
            val constsVal = negconstregex.findAllIn(currStrVal)
            if(constsVal.hasNext){
              val stringVal = constsVal.next()
              index+= stringVal.length()
              FExp(Const(const.toDouble),Const(stringVal.toDouble))
            }else if(input(index+1) == '('){
              index+=1
              FExp(Const(const.toDouble),EP(T(Const(-1),Some(TE(T(parseF(), None), '*'))), None))
            }else{
              //we have a variable letter
              val varString = varregex.findAllIn(currStrVal)
              index += varString.length
              FExp(Const(const.toDouble),Var(varString.next()))
            }


          }
        }else{
          Const(const.toDouble)
        }
      }else{
        Const(const.toDouble)
      }
    }else if(input(index) == '('){
      println("starting parse of parenthesized expression")
      index+=1
      val nested_expression = EP(parseT(false), parseETail())
      index+=1
      if(index<= input.length-1){
        if(input(index) == '^'){
          index+=1
          if(input(index) != '-') {
            FExp(nested_expression, parseF())
          }else{
            //If we have a negative value put together with the exponent, there's a few things we need to do.
            //first we'll check if this is just a negative number
            //if the character after the current index is a '(', then we really have (-1*EP)
            //if the character after the current index is a variable letter, then we really have (-1*var)
            val negconstregex: Regex = "^\\-[0-9]+(\\.[0-9]+)?".r
            val currStrVal = input.substring(index)
            val constsVal = negconstregex.findAllIn(currStrVal)
            if(constsVal.hasNext){
              val stringVal = constsVal.next()
              index+= stringVal.length()
              FExp(nested_expression,Const(stringVal.toDouble))
            }else if(input(index+1) == '('){
              index+=1
              FExp(nested_expression,EP(T(Const(-1),Some(TE(T(parseF(), None), '*'))), None))
            }else{
              //we have a variable letter
              val varString = varregex.findAllIn(currStrVal)
              index += varString.length
              FExp(nested_expression,Var(varString.next()))
            }


          }
        }else{
          nested_expression
        }
      }else{
         nested_expression
      }
    }
    else {
      val vars = varregex.findAllIn(currStr)
      val varname = vars.next()
      index += varname.length()
      //println("Index at F")
      //println(index)
      Var(varname)
      if(index<= input.length-1){
        if(input(index) == '^'){
          index+=1
          if(input(index) != '-') {
            FExp(Var(varname), parseF())
          }else{
            //If we have a negative value put together with the exponent, there's a few things we need to do.
            //first we'll check if this is just a negative number
            //if the character after the current index is a '(', then we really have (-1*EP)
            //if the character after the current index is a variable letter, then we really have (-1*var)
            val negconstregex: Regex = "^\\-[0-9]+(\\.[0-9]+)?".r
            val currStrVal = input.substring(index)
            val constsVal = negconstregex.findAllIn(currStrVal)
            if(constsVal.hasNext){
              val stringVal = constsVal.next()
              index+= stringVal.length()
              FExp(Var(varname),Const(stringVal.toDouble))
            }else if(input(index+1) == '('){
              index+=1
              FExp(Var(varname),EP(T(Const(-1),Some(TE(T(parseF(), None), '*'))), None))
            }else{
              //we have a variable letter
              val varString = varregex.findAllIn(currStrVal)
              index += varString.length
              FExp(Var(varname),Var(varString.next()))
            }


          }
        }else{
          Var(varname)
        }
      }else{
        Var(varname)
      }
    }
  }
}

object Main{
  def main(args: Array[String]): Unit ={
    //E->T [E2]|T [E3]
    //E2-> '+' E
    //E3-> '-' E
    //T->F [TE]
    //TE-> '*' T| '/' T
    //F->'('E')'|var|const|FExp|Sin(E)
    //FExp -> F'^'F
    //we're gonna use case classes just b/c they include the to-string method from the get-go
    //^\-?[0-9]+(\.[0-9]+)?|^\-?[0-9]+(\.[0-9]+)? (potential regex for negative numbers)
    //val expr = new full_expression_parser("x+(92*x^(5.97264*5^(x*5^(x+9)))/2)/-54*(2*-x)/54+7")
    try {
      print("Expression? ")
      val exprVal = scala.io.StdIn.readLine()
      if (exprVal == "quit") {
        System.exit(0)
      }
      val expr = new full_expression_parser(exprVal)
      val x = expr.parseE()
      val x_parts = x.eval()
      println(x)
      x.eval()
      x.compute()
      println(x.getIntegrationVal())
      println()
      //x.compute()
      println("Done computing")
      Main.main(args)
    }
    catch {
      case e: Exception =>
        println("Something went wrong")
        Main.main(args)
    }
  }
}