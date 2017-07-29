package functional


trait Semigroup[A] {
  def op[A](a1: A, a2: A): A
}
