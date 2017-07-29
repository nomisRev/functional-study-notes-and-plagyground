package functional

import scala.annotation.tailrec

// Define the usual List API.
//
// * Constructors (on the object)
// nil
// cons
// * methods
// map
// flatMap
// foldLeft
// filter
// reverse
// append
// any other useful functions

sealed trait List[+A] {

  // aka foldRight
  def fold[B](b: B)(f: (A, B) => B): B = this match {
    case Nil => b
    case Cons(h, t) => f(h, t.fold(b)(f))
  }

  def map[B](f: A => B): List[B] = fold(Nil: List[B])((a, b) => Cons(f(a), b))

  def append[B >: A](bs: List[B]): List[B] = fold(bs)((a, b) => Cons(a, b))

  def ::[B >: A](b: B): List[B] = Cons(b,this)

  def :::[B >: A](bs: List[B]): List[B] = bs.append(this)

  def flatMap[B](f: A => List[B]): List[B] = fold(Nil: List[B])((a, b) => f(a).append(b))

  def filter(f: A => Boolean): List[A] = flatMap(a => if (f(a)) Cons(a, Nil) else Nil)
  //fold(Nil: List[A])((a,b) => if (f(a)) Cons(a, Nil).::(b) else Nil.::(b))

  @tailrec final def foldLeft[B](b: B)(f: (B,A) => B): B = this match {
    case Nil => b
    case Cons(h,t) => t.foldLeft(f(b,h))(f)
  }

  def length: Int = fold(0)((_, b) => 1 + b)

  def reverse(): List[A] = foldLeft(Nil: List[A])((b,a) => Cons(a,b))
//    fold(Nil: List[A])((a,b) => b.append(Cons(a,Nil)))

}

case class Cons[A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

