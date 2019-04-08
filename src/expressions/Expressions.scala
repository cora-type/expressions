package expressions
import scala.collection.mutable.ListBuffer

object Expressions {


  def foreach[A](f: A ⇒ Unit): Unit

  def isAllDigits(x: String): Boolean = x forall Character.isDigit

  def evaluateArithmetic(expression:String): Double = {
    val operators: List[String] = List("*", "/", "+", "-")
    var operatorStack = new ListBuffer[String]()
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
    while(size > 1){
      for(i <- 1 to size){ // For tokens in token List
        if(isAllDigits(tokenBuffer(i))){
          queue = queue :+ tokenBuffer(i)
        } else if(i == 1){

        }
      }
    }



  }


  //def evaluate[A](evaluate:String, f:String => A, map:Map[String, (A, A) => A],list: List[List[String]]): Unit = {

  //}

  def main(args: Array[String]): Unit = {
    val test: String = "4 + 18/ ( 9 - 3 ) "
    println(evaluateArithmetic(test))
  }
}
