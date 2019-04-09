package expressions
import scala.collection.mutable.ListBuffer

object Expressions {


  def isAllDigits(x: String): Boolean = x forall Character.isDigit

  def precedence(y: String): Double = {
    /*val eval: List[List[String]] = List(List("^"), List("*", "/"), List("+", "-"))
    val pow = (a: Double, b: Double) => Math.pow(a, b)
    val mul = (a: Double, b: Double) => a * b
    val div = (a: Double, b: Double) => a / b
    val add = (a: Double, b: Double) => a + b
    val sub = (a: Double, b: Double) => a - b

    val operatorTable: Map[String, (Double, Double) => Double] = Map(
      "^" -> pow,
      "*" -> mul,
      "/" -> div,
      "+" -> add,
      "-" -> sub*/
    var x: Int = 0

    if(y == "^"){
      x = x +1
    } else if (y == "*" || y == "/"){
      x = x + 2
    } else if (y == "3" || y == "+"){
      x = x + 3
    }

    x
  }

  def evaluateArithmetic(expression:String): Double = {

    //val eval: List[List[String]] = List(List("^"), List("*", "/"), List("+", "-"))

    var operatorStack = new ListBuffer[String]
    var queue = new ListBuffer[String]

    // Removes spaces from input expression
    var y: String = expression.filterNot((x: Char) => x.isWhitespace)

    //For characters in expression string, turn "character" into  _"character"_
    for (e <- y){
      val z: String = e.toString
      y = y.replace(z, "_" + z + "_")
    }

    // split by _ then makes a list, then removes whitespaces from list
    val trimmedList: List[String] = y.split("_").map(_.trim).toList
    val tokens = trimmedList.filter(_ != "")

    val tokenBuffer = tokens.to[ListBuffer]
    val size: Int = tokenBuffer.size
    //Shunting Yard
    while(size > 1) {
      for (i <- tokenBuffer) { // For tokens in token List
        if (isAllDigits(i)) {
          queue = queue :+ i
        } else if (!isAllDigits(i)) { // If token is an operator (not a number)
          while (precedence(operatorStack.last) >= precedence(i) && operatorStack.last != "("){
            queue ++ operatorStack.reverse
            operatorStack.clear()
          }
          operatorStack = operatorStack :+ i
        } else if (i == "("){
          operatorStack = operatorStack :+ i
        } else if( i == ")"){
          while(operatorStack.last != "("){
            operatorStack.tail
          }
        }
      }
    }

    /*This is a list of lists of operators. Any operator appearing in an earlier list than
another operator will have higher precedence (evaluated first). Operators in
the same list will have equal precedence (ex. For arithmetic this would be
List(List(“^”), List(“*”, “/”), List(“+”, “-”))*/


  }


  //def evaluate[A](evaluate:String, f:String => A, map:Map[String, (A, A) => A],list: List[List[String]]): A = {

  //}

  def main(args: Array[String]): Unit = {
    val test: String = "4 + 18/ ( 9 - 3 ) "
    println(evaluateArithmetic(test))
  }

}
