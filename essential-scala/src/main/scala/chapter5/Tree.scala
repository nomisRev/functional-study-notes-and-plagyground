package chapter5

sealed trait Tree[+A] {

   def fold[B](f: A => B)(acc: (B,B) => B): B = this match {
    case Leaf(a) => f(a)
    case Node(l,r) => acc(l.fold(f)(acc),r.fold(f)(acc))
  }

}

final case class Leaf[A](value: A) extends Tree[A]
final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]