package lectures.part2afp

object LazyEval extends App{
  /*
  Evaluates the expresison until it is used
  Evaluates an expresion only once
   */

  lazy val x: Int = throw new Exception("Boom")

  lazy val ex = {
    println("Evaluating")
    2 + 2
  }
  println(ex)
  //Doesnt evaluate the expression again, it prints the sored value
  println(ex)
  //Carefull with using lazy vals w/side effects. as sometimes the compiler will not evaluate the expression if it is not needed
  //Example:

  def sideEffectingFunction: Boolean  = {
    println("Eval")
    true
  }
  def simpleFunction: Boolean = false

  println(simpleFunction && sideEffectingFunction)
  //"Eval" is not printed because simple function si already false, so && is false thus, sideEffectingFunction is never evaluated
  //Example os ussage:

  def byNameMethod(n: => Int): Int = {
    lazy val t = n
    t + t + t + t + 1 //Evaluates n only once and then the stores val is used. (sort of cachig?)
    //n + n + n + n + 1
  }


  def retrieveMagicalNumber: Int = {
    println("Waiting")
    Thread.sleep(1000)//Simulationg long computation
    50
  }
  println(byNameMethod(retrieveMagicalNumber))//Does not make sense to evaluate byNameMethod every time is used
  
  //for comprehensions are lazy evaluations.
  


}
