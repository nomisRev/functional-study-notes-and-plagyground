# Polymorphic Functions

## Parametric polymorphism

A function is parametric polymorphic when the type of a **parameter** contains one or more (unconstrained) type variables, so that the parameter may adopt any type that results from constructing a new type by passing a type variable to the type constructor.

###### Example

`def head[A](xs: List[A]): A = xs(0)` --> `A` being the type variable and `List[_]` the type constructor.

We can get the head from *any* type (List) that was constructed by calling the type constructor with a type variable.

## Subtype polymorphism

For a given `def plus[A](a1: A, a2: A): A` depending on the type of A we need a different definition/implementation. For a function `plus` we could define a `trait Plus[A]` that requires a definition for `plus`. We can now change the function signature of our original method plus to require A to be a subtype of our trait and thus demand A add-able with another A.

```
trait Plus[A] {
  def plus(a2: A): A
}

def plus[A <: Plus[A]](a1: A, a2: A): A

case class MyOwnType(message: String) extends Plus[MyOwnType] {
  override def plus(a2: MyOwnType): MyOwnType = MyOwnType(message + a2.message)
}


==> scala> MyOwnType("Hello").plus(MyOwnType(", World!"))
    res0: MyOwnType = MyOwnType(Hello, World!)
```

So in order to achieve this type of polymorphism we need to subtype our types with the trait and this needs to be done at definition time. So this is not possible for types such as Int, String,...

## Ad-Hoc polymorphism

You can solve the problem of subtype polymorphism by providing the trait definition outside of the class and passing it to the function as an (implicit) parameter.

```
trait Plus2[A] {
   def plus(a1: A, a2: A): A
 }

 object IntPlus2 extends Plus2[Int] {
   override def plus(a1: Int, a2: Int): Int = a1 + a2
 }

 implicit val intPlus2 = IntPlus2

 def plus[A](a1: A, a2: A)(implicit p: Plus2[A]): A = p.plus(a1, a2)


 ==> scala> plus(1,2)
     res1: Int = 3
```

This is truly ad-hoc:
  1. We can provide separate function definitions for different types of A
  2. We can provide function definitions to types (like Int) without access to its source
  3. The function definitions can be enabled or disabled in different scopes
