package lectures.part1as
import scala.util.Try

object DarkSugar extends App{
  //Methods w/single param
  def singleArgMethod(x: Int): String = s"$x little ducks"

  val descrption = singleArgMethod {
    val x = 5
    val y = 6
    y + x
  }
  println(descrption)

  val aTryInstance = Try {
    throw new Exception("Boom")
  }

  List(1,2,3).map{
    x => x + 1
  }
  //Instances of traits w single methods can be converted into lambdas
  //Single Abstract Method
  trait Action {
    def act(x: Int): Int

  }
  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }
  //Hace override de act
  val aLambda: Action = (x: Int) => x + 1
  val aThreade = new Thread {
    override def run(): Unit = println("Hello")
  }
  val aSweetThread = new Thread(() => println("Hello"))

  abstract class AnAbstractType{
    def implemented: Int = 23
    def f(x: Int): Unit
  }
  val anAbstractInstance: AnAbstractType = (a: Int) => println(a)

  val prependedList = 2::List(1,2,3)

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
  }

  val myStream = 1 -->:  2 -->: new MyStream[Int]

  class TeenGirl(name: String) {
    def `and then said` (gossing: String): Unit = println(s"$name said $gossing")
  }
  //Rarely use
  val lily = new TeenGirl("Lily")
  lily `and then said` "Hello"
  //infix types
  class Composite[A,B]
  val composite: Int Composite String = ???



}


