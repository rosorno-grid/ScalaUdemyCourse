package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {
  def apply(a: A): Boolean = contains(a)
  def contains(a: A): Boolean
  def +(a: A): MySet[A] // add element
  def ++(other: MySet[A]): MySet[A] //Concat (Union)
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(f: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
}
class EmptySet[A] extends MySet[A]{
  def contains(a: A): Boolean = false
  def +(a: A): MySet[A] = new NonEmptySet(a, this)
  def ++(other: MySet[A]): MySet[A] = other

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(f: A => Boolean): MySet[A] = this

  def foreach(f: A => Unit): Unit = ()
}

class NonEmptySet[A](val head: A, val tail: MySet[A]) extends MySet[A]{
  def contains(a: A): Boolean =
    a == head || tail.contains(a)

  def +(a: A): MySet[A] =
    if (contains(a)) this
    else new NonEmptySet(a, this)

  def ++ (other: MySet[A]): MySet[A] =
    if (other.contains(head)) this
    else new NonEmptySet(head, tail ++ other)

  def map[B](f: A => B): MySet[B] =
    new NonEmptySet(f(head), tail.map(f))

  def flatMap[B](f: A => MySet[B]): MySet[B] =
    f(head) ++ tail.flatMap(f)

  def filter(f: A => Boolean): MySet[A] = {
    if (f(head)) new NonEmptySet(head, tail.filter(f))
    else tail.filter(f)
  }
  // if curret (head) is valid
  //add current to new set
  //else
  //dont add current, continue with tail
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

}
object MySet {
  def apply[A](as: A*): MySet[A] =
    @tailrec
    def buildSet(valSeq: Seq[A], acc : MySet[A]): MySet[A] =
      valSeq match {
        case Seq() => acc
        case Seq(h, t*) => buildSet(t, acc + h)
      }
    buildSet(as.toSeq, new EmptySet[A])
}
