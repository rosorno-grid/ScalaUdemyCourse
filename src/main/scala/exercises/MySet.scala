package exercises

import javax.print.attribute.standard.MediaSize.Other
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

  def -(a: A): MySet[A]
  def --(other: MySet[A]): MySet[A]
  def &(other: MySet[A]): MySet[A]

}
class EmptySet[A] extends MySet[A]{
  def contains(a: A): Boolean = false
  def +(a: A): MySet[A] = new NonEmptySet(a, this)
  def ++(other: MySet[A]): MySet[A] = other

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(f: A => Boolean): MySet[A] = this

  def foreach(f: A => Unit): Unit = ()

  def -(a: A): MySet[A] = this
  def --(other: MySet[A]): MySet[A] = this
  def &(other: MySet[A]): MySet[A] = this
}

class NonEmptySet[A](val head: A, val tail: MySet[A]) extends MySet[A]{
  def contains(a: A): Boolean =
    a == head || tail.contains(a)

  def -(a: A): MySet[A] =
    if (a == head) tail
    else new NonEmptySet(head, tail - a)

  def &(other: MySet[A]): MySet[A] =
    filter(other)

  def --(other: MySet[A]): MySet[A] =
    filter(a => !other.contains(a))

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

//a set build of all elements A that satify a property
//property: A => Boolean
//{x in A | property(x)}
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A]{
    def contains(a: A): Boolean = property(a)  //a belongs to a set it the property is true for the element (a)

    //{x in A | property(x) + elements} = {x in A | property(x) || x == element}
    def +(a: A): MySet[A] = {
      new PropertyBasedSet[A](x => property(x) || x == a)
    }

    def ++(other: MySet[A]): MySet[A] = {
      new PropertyBasedSet[A](x => property(x) || other.contains(x))
    }
    //map, flatmap y foreach on rabbit holes al procesar conjuntos potencialmente infinitos
    def map[B](f: A => B): MySet[B] =  failedException
    def flatMap[B](f: A => MySet[B]): MySet[B] =  failedException
    def foreach(f: A => Unit): Unit =  failedException

    def filter(f: A => Boolean): MySet[A] =  new PropertyBasedSet[A](x => property(x) && f(x))

    def &(other: MySet[A]): MySet[A] =  filter(other)

    def failedException = throw new Exception("Failed")

    override def -(a: A): MySet[A] = ???

    override def --(other: MySet[A]): MySet[A] = ???
}