package lectures.part1as

object AdvancedPatternMatching extends App{

  val numbers = List(1)
  object AllThePaterns

  val x: Any = "Scala"
  val constants = x match{
    case 1 => "One"
    case "Scala" => "Two"
    case AllThePaterns => "Three"

  }
  val matchAVariable = x match {
    case 1 => "One"
    case anything => s"Anything else $anything"
  }

  //The difference between usinv a variable and a _ is that the variable bings the pattern
  val nestedTuples = (2, (3,4))
  val matchNested = nestedTuples match {
    case (v, (3,_)) => "Matched"
    case _ => "Not matched"
  }
  //Type specifier
  val unknownType: Any = 1
 
  val aList = List(1,2,3)
  //Name binging
  val nameBinding = aList match{
    case nonEmpty @ _ :: _ => nonEmpty
    case Nil => Nil  
  }
  //if - guards para filtrar 
  val specialElement = nestedTuples match{
    case (v, (3, _)) if v % 2 == 0 => println("Even")
    case _ => println("Odd")
  }
  
  //Advanced features
  
  val numbers2 = List(1)
  val describtion = numbers match {
    case head :: Nil => println(s"The head is $head")
    case _ => println("Not a list")
  }
  //Custom pattern matching
  //The Person on the Match does not have any to do with the Person class
  //The singleton object Person is the pattern
  //the unapply method is the extractor
  class Person(val name: String, val age: Int)
  object Person {
    def unapply(person: Person): Option[(String, Int)] = Some((person.name, person.age))
  }
  val bob = new Person("Bob", 23)
  val greeting = bob match {
    case Person(name, age) => s"Hello, my name is $name and I am $age years old"
  }
  /*
  Exercise
   */
  object singleDigit {
    def unapply(x: Int): Option[Int] = if (x > 9 && x < 0) None else Some(x)
  }
  object even {
    def unapply(x: Int): Option[Int] = if (x % 2 == 0) Some(x) else None
  }
  val n: Int = 1
  val matchProperty = n match{
    case singleDigit(x) => println(s"Single digit $x")
    case even(x) => println(s"Even $x")
    case _ => println("Not a single digit or even")  
  }
  //decomposing sequence
  abstract class Mylist[+A] {
    def head: A = ???
    def tail: Mylist[A] = ???
    
  }
  case object Empty extends Mylist[Nothing]
  case class Cons[+A](override val head: A, override val tail: Mylist[A]) extends Mylist[A]
  object Mylist {
    def unapplySeq[A](mylist: Mylist[A]): Option[Seq[A]] =
      if (mylist == Empty) Some(Seq.empty)
      else unapplySeq(mylist.tail).map(mylist.head +: _)
  }
  val mylist = Cons(1, Cons(2, Cons(3, Empty)))
  val matchMylist = mylist match {
      case Mylist(1, 2, _*) => println(s"Starting 1 2")
  }
}
