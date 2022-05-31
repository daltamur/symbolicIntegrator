import grammarClasses.{Const, E, E2, E3, EP, F, FExp, S, T, TE, Var}

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

//grammarClasses.S->grammarClasses.E
//grammarClasses.E-> Term Term_Tail
//Term-> factor factor_tail
//this keeps addition and subtraction towards the top of the tree, so they are done last
//Term_Tail->'+'grammarClasses.E|'-'grammarClasses.E|null
//factor-> '('grammarClasses.E')'|var|num
//this puts multiplication and division to the bottom of the tree, so it is done last
//factor_tail-> '*'Term

//Doing it this way isn't really conducive of a tree structure...here's a better grammar:
//Though this is arguably an ambigiuous grammar, it will get our job done!
//things in brackets represent an optional token
// grammarClasses.S = Statement
// grammarClasses.E = Expression
// grammarClasses.T = Terminal
// grammarClasses.F = Factor
// var = variable
// const = constant value (some number)
//___
// for the sake of this project, we're not going to worry about accepting decimal numbers (after all, this is for
// symbolic integration)

// Later on, we're going to have it accept exponents and negative numbers though (I was just struck with inspiration on
// how to do this and need to work those into terminal/factor tokens...

class full_expression_parser(input: String) {
  val constregex: Regex = "^(\\-|(\\d(\\.))?)\\d+(\\.\\d+)?".r
  val varregex: Regex = "^[A-Za-z]+".r

  //this will serve as our incrementer in parsing the expression
  var index = 0

  def parseS(): S = parseE()

  def parseE(): E = E(parseT(false), parseETail())

  def parseETail(): Option[Either[E2, E3]] = {
    //if it is adding, then we have an grammarClasses.E2 class
    if (index < input.length && input(index) == '+'){
      //println("index at ETail")
      index+=1; // Advance past +
      //println(index)
      Some(Left(E2(parseE())))
    //if it is subtracting, then we have an grammarClasses.E3 class
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
    //if it is multiplying, then we have an grammarClasses.E2 class
    if (index < input.length && input(index) == '*'){
      index+=1; // Advance past *
      Some(TE(parseT(false), '*'))
      //if it is dividing, then we have an grammarClasses.E3 class
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
      //println("index at grammarClasses.F")
      //println(const.toInt)
      //println(index)
      Const(const.toDouble)
      if(index <= input.length-1){
        if(input(index) == '^'){
          index+=1
          if(input(index) != '-') {
            grammarClasses.FExp(Const(const.toDouble), parseF())
          }else{
            //If we have a negative value put together with the exponent, there's a few things we need to do.
            //first we'll check if this is just a negative number
            //if the character after the current index is a '(', then we really have (-1*grammarClasses.EP)
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
      val nested_expression = grammarClasses.EP(parseT(false), parseETail())
      index+=1
      if(index<= input.length-1){
        if(input(index) == '^'){
          index+=1
          if(input(index) != '-') {
            FExp(nested_expression, parseF())
          }else{
            //If we have a negative value put together with the exponent, there's a few things we need to do.
            //first we'll check if this is just a negative number
            //if the character after the current index is a '(', then we really have (-1*grammarClasses.EP)
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
      //println("Index at grammarClasses.F")
      //println(index)
      Var(varname)
      if(index<= input.length-1){
        if(input(index) == '^'){
          index+=1
          if(input(index) != '-') {
            grammarClasses.FExp(Var(varname), parseF())
          }else{
            //If we have a negative value put together with the exponent, there's a few things we need to do.
            //first we'll check if this is just a negative number
            //if the character after the current index is a '(', then we really have (-1*grammarClasses.EP)
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

