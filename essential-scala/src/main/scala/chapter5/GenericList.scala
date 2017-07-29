package chapter5

import scala.annotation.tailrec

sealed trait LinkedList[+A] {

  final def apply[AA >: A](index: Int): Sum[String, AA] = {
    def go(list: LinkedList[A], count: Int): Sum[String, AA] = list match {
      case Nil => Failure("Index out of bounds")
      case Cons(h, t) => if (count == index) Success(h) else go(t, count + 1)
    }

    go(this, 0)
  }

  final def length: Int = this match {
    case Nil => 0
    case Cons(_, tail) => 1 + tail.length
  }

  def lengthByFold: Int = fold(0)((_, b: Int) => 1 + b)

  @tailrec final def contains[AA >: A](a: AA): Boolean = this match {
    case Nil => false
    case Cons(h, t) => if (h == a) true else t.contains(a)
  }

  final def fold[B](identity: B)(f: (A, B) => B): B = this match {
    case Nil => identity
    case Cons(hd, tl) => f(hd, tl.fold(identity)(f))
  }

  final def map[B](f: A => B): LinkedList[B] = fold(Nil: LinkedList[B]) { (a, b) => Cons(f(a), b) }

  final def append[AA >: A](as: LinkedList[AA]): LinkedList[AA] = this match {
    case Nil => as
    case Cons(h, t) => Cons(h, t.append(as))
  }

  final def flatMap[B](f: A => LinkedList[B]): LinkedList[B] = fold(Nil: LinkedList[B])((a, b) => f(a).append(b))

}

object LinkedList {
  def apply[A](as: A*): LinkedList[A] = as.foldRight(Nil: LinkedList[A]){ (a,b) => Cons(a,b)}

  def sum(list: LinkedList[Int]): Int = list.fold(0) { (a, b) => a + b }

  def product(list: LinkedList[Int]): Int = list.fold(1) { (a, b) => a * b }

  def double(list: LinkedList[Int]): LinkedList[Int] = list.map(a => 2 * a)
}


case object Nil extends LinkedList[Nothing] {
  //  override def fold[B](identity: B, f: (A, B) => B): B = identity
}
final case class Cons[A](head: A, tail: LinkedList[A]) extends LinkedList[A] {
  //  override def fold[B](identity: B, f: (A, B) => B): B = f(head, tail.fold(identity, f))
}