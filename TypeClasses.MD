# Type Classes in Haskell (Scalaz)

A typeclass is a sort of interface that defines some behavior. If a type is a part of a typeclass, that means that it supports and implements the behavior the typeclass describes. (A lot of people coming from OOP get confused by typeclasses because they think they are like classes in object oriented languages.) [source](http://learnyouahaskell.com/types-and-typeclasses)

## Equal

Eq is used for types that support equality testing. The functions its members implement are == and /=.
 * Scalaz equivalent for the Eq typeclass is called Equal and it enables ===, =/= and assert_===.
 * Compared to the OOP alternative Equal is typesafe and thus will result in a compile error.

## Order

Ord is for types that have an ordering. Ord covers all the standard comparing functions such as `>`, `<`, `>=` and `<=`.
 * Scalaz equivalent for the Ord typeclass is called Order.
 * Order enables `?|?` which returns ordering `LT`, `GT` or `EQ`. It also enables `lt`, `gt`, `lte`, `gte`, `min` and `max`. Similar to Equal ordering is typesafe.

 ## Show

 Show is used to represent types as a String. (functional definition of toString)

 ## Read

Read is sort of the opposite typeclass of Show. It takes a String and returns a type.
 *  Not available in Scalaz.

 ## Enum

 Enum members are sequentially orderered **types** and they can be enumerated. They also have defined successors and predecessors, available through `succ` and `pred`.

 ## Bounded

 Bounded members have an upper and a lower bound.
 => Scalaz equivalent is also Enum

 ## Num

Num is a numeric typeclass. Its members have the property of being able to act like numbers.

* No Scalaz equivalent for Num, Floating, and Integral.
