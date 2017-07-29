package functional

trait Monoid[A] extends Semigroup[A] {
  def id[A]: A
}
