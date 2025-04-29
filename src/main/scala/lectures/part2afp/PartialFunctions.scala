package lectures.part2afp

object PartialFunctions extends App {

  val aFunction = (x: Int) => x + 1 //Function1[Int:Int] === Int=>Int
  //Partial functions not accept any values, we can restrict values

  val aFussyFunction = (x : Int) =>
    if (x < 10) x + 1
    else if (x == 1) 56
    else 2

  val aFussyFunc2 = (x : Int) => x match {
    case 1 => 56
    case _ => x + 1
  }
   val aPartialFunction: PartialFunction[Int, Int] = {
     case x if x < 10 => x + 1
     case 1 => 56
     case 8 => 2
   }//partial function value
  println(aPartialFunction.isDefinedAt(10))//Test para ver si esta definida para x vaor
  //lift
  val aLiftedFunction = aPartialFunction.lift// Int => Option[Int] usefull to validate and protect partianl functions

  val aChainPartialFunc = aPartialFunction.orElse[Int, Int] {
    case x if x > 10 => x + 1
  }
  //PF extends normal functions
  //HOFs accept partial functions as well

  val aMappedList = List(1,2,3).map{
    case 1 => 56
    case 2 => 2
    case 3 => 5
      case _ => 10
  }
  /*
  Exercises
  1- construct a PF instance (Anonymous class)
  2- Build a dump chatbot
   */

  val aManualFussyFunction = new PartialFunction[Int, Int] {
    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x == 3

    override def apply(v1: Int): Int = v1 match{
      case 1 => 56
      case 2 => v1 + 1
      case 3 => v1 + 3
    }
  }

  val chatbot: PartialFunction[String, String] = {
    case "Hi" => "Hello"
    case "Bye" => "Goodbye"
    case "What's up" => "I'm fine"
    case _ => "I don't know"
  }
  scala.io.Source.stdin.getLines().foreach(line => println(chatbot(line)))

}

