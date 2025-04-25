package lectures.part1as

import scala.annotation.tailrec

object Recap extends App{

  /*
  Scala is about exxressions
   */
  val aConditions: Boolean = false
  val aConditionsVal = if(aConditions) 42 else 65
  /*
  Instructions vs expresions
  Instruccions: Imperative languages, sequential to do things
  Expresisions: Buidl expressions on top of other Expressions
   */
  val aCodeBlock = {
    val a = 3
    val b = 5
    a + b // aCodeBlock == 8
  }

  /*
  UNIT= only do side efects. Equivalent to void
   */
  def aFunction(x: Int): Int = aCodeBlock + 1
  //In order to avoid stackOVerflow use tail recursion

  @tailrec def factorial(n: Int, acc: Int): Int = {
    if (n <= 0) return acc
    else factorial (n-1, acc * n)
  }
  //Functional programing

  val otherIncrementer = (x: Int) => x +1
  List(1, 2 ,3).map(otherIncrementer)

  val pairs = for{
    num <- List(1, 2, 3)
    str <- List("1","b", "3")
  } yield num + "-" + str
}
