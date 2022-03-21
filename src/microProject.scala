/*
import scala.util.matching.Regex

// Unambiguous grammar
// E -> T + E | T
// T -> Const | Var

// Easier to parse grammar
// S -> E$
// E -> Terminal E2
// E2 -> + E
// E2 -> NIL
// Terminal -> Const
// Terminal -> Val

abstract class S {
  def eval(env: Main.Environment): Int
}


abstract class Terminal extends S


case class E(l: Terminal, right: Option[E2]) extends S {
  def eval(env: Main.Environment): Int = {
    val a1: Int = l match {
      case v:Var => v.eval(env)
      case c:Const => c.eval(env)
    }
    right match {
      case Some(r) => a1 + r.eval(env)
      case None => a1
    }
  }
}


case class E2(l: E) extends S {
  def eval(env: Main.Environment): Int = l.eval(env)
}


case class Var(n: String) extends Terminal {
  def eval(env: Main.Environment): Int = env(n)
}


case class Const(v: Int) extends Terminal {
  def eval(env: Main.Environment): Int = v
}


class microProject(input:String) {
  val constregex: Regex = "^[0-9]+".r
  val varregex: Regex = "^[A-Za-z]+".r

  var index = 0

  def parseS(): S = parseE()

  def parseE(): E = E(parseTerminal(), parseE2())

  def parseE2(): Option[E2] = {
    if (index < input.length && input(index) == '+'){
      index+=1; // Advance past +
      Some(E2(parseE()))
    }
    else None
  }

  def parseTerminal(): Terminal = {
    // Get the unparsed part of the string.
    val currStr = input.substring(index)

    // Get either the const or var which is there.
    val consts = constregex.findAllIn(currStr)
    if (consts.hasNext){
      val const: String = consts.next()
      index += const.length()
      Const(const.toInt)
    }
    else {
      val vars = varregex.findAllIn(currStr)
      val varname = vars.next()
      index += varname.length()
      Var(varname)
    }
  }
}


object Main {
  type Environment = String => Int

  def main(args: Array[String]){
    val env: Environment = { case "x" => 5 case "y" => 7 }

    val rd = new microProject("x+x+7+y")
    val exp2rd:S = rd.parseE()
    println(exp2rd)
    println(exp2rd.eval(env))
  }
}

 */

