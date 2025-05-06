package exercises

object myStream extends App {

  abstract class MyStream[+A] {
    def isEmpty: Boolean

    def head: A

    def tail: MyStream[A]

    def #::[B >: A](x: B): MyStream[B]

    def ++[B >: A](s: MyStream[B]): MyStream[B] //concat

    def foreach(f: A => Unit): Unit

    def map[B](f: A => B): MyStream[B]

    def flatMap[B](f: A => MyStream[B]): MyStream[B]

    def filter(f: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A] //takes the first n elemts from the stream

    def takeAsList(n: Int): List[A]

  }

  object EmptyStream extends MyStream[Nothing] {
    def isEmpty: Boolean = true

    def head: Nothing = throw new Exception("Empty Stream")

    def tail: MyStream[Nothing] = throw new Exception("Empty Stream")

    def #::[B >: Nothing](x: B): MyStream[B] = new Cons(x, this)

    def ++[B >: Nothing](s: MyStream[B]): MyStream[B] = s

    def foreach(f: Nothing => Unit): Unit = ()

    def map[B](f: Nothing => B): MyStream[B] = this

    def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

    def filter(f: Nothing => Boolean): MyStream[Nothing] = this

    override def take(n: Int): MyStream[Nothing] = this

    override def takeAsList(n: Int): List[Nothing] = Nil
  }

  class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
    def isEmpty: Boolean = false

    override val head: A = hd
    override lazy val tail: MyStream[A] = tl //call by need

    def #::[B >: A](x: B): MyStream[B] = new Cons(x, this)

    def ++[B >: A](s: MyStream[B]): MyStream[B] = new Cons(hd, tail ++ s)

    def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }

    def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f))

    def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

    def filter(f: A => Boolean): MyStream[A] = {
      if (f(head)) new Cons(head, tail.filter(f))
      else tail.filter(f)
    }

    def take(n: Int): MyStream[A] = {
      if (n == 0) EmptyStream
      else new Cons(head, tail.take(n - 1))
    }

    def takeAsList(n: Int): List[A] = {
      if (n == 0) Nil
      else head :: tail.takeAsList(n - 1)
    }
  }

  object MyStream {
    def from[A](n: A)(generator: A => A): MyStream[A] =
      new Cons(n, MyStream.from(generator(n))(generator))
  }
}

object playground extends App{
  import myStream._
  val stream  = MyStream.from(1)(_ + 1) //Not crash because tail is lazyly evaluated
  //stream.foreach(println) crash beacuse evaluates all natural numbers(infinite)
  println(stream.take(10).takeAsList(10)) //not crash because it only evaluates 10 elements and peform a "Cast" from stream to list
  println(stream.head)
  println(stream.tail.head)
  println(stream.tail.tail.head)// As it is lazly evaluated, it will not evaluate tail is not necessary
}

