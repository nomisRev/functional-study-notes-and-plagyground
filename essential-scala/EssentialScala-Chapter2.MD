# Essential Scala - Chapter 2 Expressions, Types, and Values

When we evaluate a program in the Scala console or worksheet we are told two pieces of information: the type of the program, and the value it evaluates to.

```scala
scala> "Hello world!"
res0: String = Hello world!
```

In this case the type is String and the value is "Hello world!".

A Scala program exists out of:
  * Expressions are the parts of a program that evaluate to a value. They are the major part of a Scala program.
  * Expressions have types, which express some restrictions on programs. During compile-time the types of our programs are checked. If they are inconsistent then compilation fails and we cannot evaluate, or run, our program.
  * Values exist in the computer’s memory, and are what a running program manipulates.

## Values vs Types

  * A value is information stored in the computer’s memory. It exists at run-time.
  * Types are restrictions on our programs that limit how we can manipulate objects.

  `expressions have types but values do not` => We cannot inspect an arbitrary piece of the computer’s memory and divine how to interpret it without knowing the program that created it. For example, in Scala the Int and Float types are both represented by 32-bits of memory.

We can show that types exist at compile-time by asking the Scala console to tell us the type of an expression that causes a run-time error.

```scala
scala> :type 2/0
Int

scala> 2/0
java.lang.ArithmeticException: / by zero
```
We see that the expression 2 / 0 has type Int even though this expression fails when we evaluate it.

**Type erasure**
* Types, which exist at compile-time, restrict us to writing programs that give a consistent interpretation to values. We cannot claim that a particular 32-bits is at one point an `Int` and another a `Float`. When a program type checks, Scala guarantees that all values are used consistently and thus it does not need to record type information in a value’s representation. This process of removing type information is called type erasure.

## Objects

An object is a grouping of data and operations on that data. For example, 2 is an object. The data is the integer 2, and the operations on that data are familiar operations like + , - , and so on. **The operations are known as methods. The data is stored in fields.**

**Method call syntax**
* `anExpression.methodName(param1, ...)`
* `anExpression.methodName` <= parentheses are optional when there are no parameters

In contrast to Java is every value in Scala an object; we can call methods on primitive types such as Int and Boolean.


But if an Int is an object, what are the basic mathematical operators such as + and - ? Are they also methods? Yes—Scala methods can have symbolic names as well as alphanumeric ones!

```scala
scala> 43 - 3 + 2
res7: Int = 42

scala> 43.-(3).+(2)
res8: Int = 42
```

**Infix Operator Notation**
* Any Scala expression written a.b(c) can also be written a b c . Note that a b c d e is equivalent to a.b(c).d(e) , not a.b(c, d, e)

**Scala vs Java’s Type Hierarchy**
* Although they are written with initial capitals, Scala’s `Int` , `Double` , `Float` , `Long` , `Short` , `Byte` , `Boolen` and `Char` refer to exactly the same things as `int` , `double` , `float` , `long` , `short` , `byte` , `boolean` , and `char` in Java. In Scala all of these types act like objects with methods and fields. However, once your code is compiled, a Scala `Int` is exactly the same as a Java `int` . This makes interoperability between the two languages a breeze.

**Using Nulls in Scala**
* Although nulls are common in Java code, they are considered very bad practice in Scala. Scala’s `null` also has its own type: `Null`.

**Unit**
* `Unit`, written `()` , is the Scala equivalent of Java’s `void`.

## Object literals
* When we write an object literal we use a declaration, which is a different kind of program to an expression. A declaration does not evaluate to a value. Instead is gives a name to a value.

```
scala> object Test { }
defined object Test

scala> Test
res14: Test.type = Test$@77b5148c
```

Note that the type of the object is reported as Test.type. This is not like any type we’ve seen before it’s a new type, created just for our object, called a singleton type. **We cannot create other values of this type.**

Within the body (between the braces) of an object declaration we can put expressions. It is more common, however, to put declarations such as declaring methods, fields, or even more objects.

#### Methods
* `def name (parameter: type , ...): resultType = bodyExpression`
* `def name: resultType = bodyExpression`

Method parameters are optional, but if a method has parameters their type must be given. Although the result type is optional it is good practice to define it as it serves as (machine checked!) documentation. The term argument may be used interchangeably with parameter .

**return is implicit:** The return value of the method is determined by evaluating the body - there is no need to type the `return` keyword.

#### Fields
An object can also contain other objects, called fields . We introduce these using the keywords `val` or `var`.

* val name: type = valueExpression
* var name: type = valueExpression

=> Using val defines an immutable field, meaning we cannot change the value bound to the name. A var field is mutable , allowing us to change the bound value.

#### Process for writing methods
1. Identify the type of the inputs and output of the method.
2. Write some test cases for the expected output of the method given example input.
3. Write the method declaration using `???` => `def square(x: Double): Double = ???`
4. Run the code to check if the tests do in fact fail.
5. Write the body of the method using the following 2 methods:
    * Consider the result type and how we can create an instance of it
    * Consider the input type and methods we can call to transform it to the result type.
6. Run the tests again and check the test cases pass.

## Compound Expressions

**Conditions are Expressions** Scala's `if` statement has a type and a return type.
w
**Block** is a sequence of expressions or declarations surrounded by curly braces. A block is also an expression: it executes each of its sub-expressions in order and returns the value of the last expression.

# Extra take aways

**Always prefer val to var.**

**Objects and classes (which we’ll see later) aren’t loaded until they are referenced by other code. This is what prevents Scala loading the entire standard library into memory to run a simple "Hello world!" app.**

**Type information is not complete and there are occasions where it is lost. Blurring the distinction between compile- and run-time is thus dangerous. If we never rely on type information being around at run-time (and the patterns we will show you do not) we will never run into these issues.**
