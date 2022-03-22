import java.awt.Choice
import java.util
import scala.collection.mutable
import scala.util.control.Breaks.break
import scala.util.matching.Regex

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
//FExp -> F'^'F
//we're gonna use case classes just b/c they include the to-string method from the get-go

abstract class S{
  // some sort of abstract function would go here
  def eval()
}

abstract class F extends S

case class E(l: T, r: Option[Either[E2, E3]]) extends S{
  override def eval(): Unit = {
    //print("<Start E>")
    l.eval()
    r match {
      case Some(Left(r)) => r.eval()
      case Some(Right(r)) => r.eval()
      case None =>
    }
    //print("<End E>")
  }
}

case class EP(l: T, r: Option[Either[E2, E3]]) extends F {
  override def eval(): Unit = {
    //print("<start parenthesis>")
    print('(')
    l.eval()
    r match {
      case Some(Left(r)) => r.eval()
      case Some(Right(r)) => r.eval()
      case None => //print()
    }
    print(')')
    //print("<end parenthesis>")

  }
}

case class E2(l: E) extends S{
  override def eval(): Unit = {
    print('+')
    l.eval()
  }
}

case class E3(l: E) extends S{
  override def eval(): Unit = {
    print('-')
    l.eval()
  }
}

case class T(l: F, r: Option[TE]) extends S{
  override def eval(): Unit = {
    //print("<Start T>")
    l.eval()
    r match {
      case Some(r) => r.eval()
      case None => //print()
    }

    //print("<End T>")
  }
}

case class TE(l: T, operation: Char) extends S{
  override def eval(): Unit = {
    print(operation)
    l.eval()
  }
}


case class FExp(l: F, r: F) extends F{
  override def eval(): Unit = {
    //print("<Start exp>")
    l.eval()
    print('^')
    r.eval()
    //print("<End exp>")
  }
}

case class Var(n: String) extends F {
  override def eval(): Unit = {print(n)}
}

case class Const(v: Double) extends F {
  //def eval(env: Main.Environment): Int = v
  override def eval(): Unit = {print(v)}
}


class full_expression_parser(input: String) {
  val constregex: Regex = "^[0-9]+(\\.[0-9]+)?".r
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
    if (isExponent) {
      T(parseF(), parseTTail())
    }else{
      T(parseF(), parseTTail())
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
          FExp(Const(const.toDouble), parseF())
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
          FExp(nested_expression, parseF())
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
          FExp(Var(varname), parseF())
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
    val expr = new full_expression_parser("(x+(92*x^(5.97264*x)))/54*(2*x)-54+7")
    val x = expr.parseS()
    println(x)
    x.eval()
  }
}