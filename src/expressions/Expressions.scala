package expressions
import scala.collection.mutable.ListBuffer

object Expressions {


  def isAllDigits(x: String): Boolean = x forall Character.isDigit

  def evaluateArithmetic(expression:String): Double = {
    val eval: List[List[String]] = List(List("^"), List("*", "/"), List("+", "-"))
    val operatorStack:  ListBuffer[String] = ListBuffer()
    var queue = new ListBuffer[String]()

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
        } else if (i == "*" || i == "/" || i == "+" || i == "-" || i == "^") {
          while (operatorStack.nonEmpty){

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
