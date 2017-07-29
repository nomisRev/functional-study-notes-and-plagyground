# Functional
## Modeling data

In the terminology of object-oriented programming, we will express data with `is-a` and `has-a` relationships. In FP data is usually modeled with logical `OR` (sum) and `AND` (product). The missing `is-a or`and `has-a and` are modeling using a combination of sum and product.


# Map

* We have a list of Int user IDs, and a function which, given a user ID, returns a User record. We want to get a list of user records for all the IDs in the list. Written as types we have List[Int] and a function Int => User, and we want to get a List[User].
* We have an optional value representing a user record loaded from the database and a function that will load their most recent order. If we have a record we want to then lookup the user’s most recent order. That is, we have a Maybe[User] and a function User => Order, and we want a Maybe[Order].
* We have a sum type representing an error message or a completed order. If we have a completed order we want to get the total value of the order. That is, we have a Sum[String, Order] and a function Order => Double, and we want Sum[String, Double].

What these all have in common is we have a type F[A] and a function A => B, and we want a result F[B]. The method that performs this operation is called map.

# FlatMap

* We have a list of users and we want to get a list of all their orders. That is, we have LinkedList[User] and a function User => LinkedList[Order], and we want LinkedList[Order].
* We have an optional value representing a user loaded from the database, and we want to lookup their most recent order—another optional value. That is, we have Maybe[User] and User => Maybe[Order], and we want Maybe[Order].
* We have a sum type holding an error message or an Order, and we want to email an invoice to the user. Emailing returns either an error message or a message ID. That is, we have Sum[String, Order] and a function Order => Sum[String, Id], and we want Sum[String, Id].

What these all have in common is we have a type F[A] and a function A => F[B], and we want a result F[B]. The method that performs this operation is called flatMap.

# Functor

A type like F[A] with a map method is called a functor.

# Monad

If a functor also has a flatMap method it is called a monad.
