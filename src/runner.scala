object runner{
    def main(args: Array[String]): Unit ={
      //grammarClasses.E->grammarClasses.T [grammarClasses.E2]|grammarClasses.T [grammarClasses.E3]
      //grammarClasses.E2-> '+' grammarClasses.E
      //grammarClasses.E3-> '-' grammarClasses.E
      //grammarClasses.T->grammarClasses.F [grammarClasses.TE]
      //grammarClasses.TE-> '*' grammarClasses.T| '/' grammarClasses.T
      //grammarClasses.F->'('grammarClasses.E')'|var|const|grammarClasses.FExp|Sin(grammarClasses.E)
      //grammarClasses.FExp -> grammarClasses.F'^'grammarClasses.F
      //we're gonna use case classes just b/c they include the to-string method from the get-go
      //^\-?[0-9]+(\.[0-9]+)?|^\-?[0-9]+(\.[0-9]+)? (potential regex for negative numbers)
      //val expr = new full_expression_parser("x+(92*x^(5.97264*5^(x*5^(x+9)))/2)/-54*(2*-x)/54+7")
      try {
        print("Expression? ")
        var exprVal = scala.io.StdIn.readLine()
        if (exprVal == "quit") {
          System.exit(0)
        }
        val expression = jas.core.Compiler.compile(exprVal)
        //In the future, we will modify this just a bit so that if the initial integration doesn't work we'll try it on an expanded
        //version of the expression
        //exprVal = (expression.expand().simplify().beautify().simplify().beautify().toString)
        exprVal = expression.simplify().beautify().toString
        println(exprVal)
        val expr = new full_expression_parser(exprVal)
        val x = expr.parseE()
        println(x.getString)
        val x_parts = x.eval()
        println(x)
        x.eval()
        x.compute()
        println(x.getIntegrationVal())
        println()
        //x.compute()
        println("Done computing")
        runner.main(args)
      }
      catch {
        case e: Exception =>
          println("Something went wrong")
          runner.main(args)
      }
    }
  }
