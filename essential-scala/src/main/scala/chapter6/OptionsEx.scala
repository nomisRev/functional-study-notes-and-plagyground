package chapter6


object OptionsEx {

  def addOptions[A, B](a1: Option[A], a2: Option[A])(f: (A, A) => B): Option[B] = for {
    first <- a1
    second <- a2
  } yield f(first, second)

  def addOptionsByMap[A, B](a1: Option[A], a2: Option[A])(f: (A, A) => B): Option[B] =
    a1.flatMap(aa => a2.map(aa2 => f(aa, aa2)))

  def addMoreOptions[A, B](a1: Option[A], a2: Option[A], a3: Option[A])(f: (A, A, A) => B): Option[B] = for {
    first <- a1
    second <- a2
    third <- a3
  } yield f(first, second, third)

  def addMoreOptionsByMap[A, B](a1: Option[A], a2: Option[A], a3: Option[A])(f: (A, A, A) => B): Option[B] =
    a1.flatMap(aa => a2.flatMap(aa2 => a3.map(aa3 => f(aa, aa2, aa3))))

  def divide(i1: Int, i2: Int): Option[Int] = if (i2 == 0) None else Some(i1 / i2)

  def divideOptions(i1: Option[Int], i2: Option[Int]): Option[Int] = for {
    ii1 <- i1
    ii2 <- i2
    result <- divide(ii1, ii2)
  } yield result

  def calculator(operand1: String, operator: String, operand2: String): Unit = {
    def readInt(str: String): Option[Int] = if (str matches "\\d+") Some(str.toInt) else None

    def performOperator(a: Int, b: Int): Option[Int] = {
      operator match {
        case "+" => Some(a + b)
        case "-" => Some(a - b)
        case "/" => divide(a, b)
        case "*" => Some(a * b)
        case _ => None
      }
    }

    val result = for {
      a <- readInt(operand1)
      b <- readInt(operand2)
      result <- performOperator(a, b)
    } yield result

    result match {
      case Some(number) => println(s"The answer is $number!")
      case None => println(s"Error calculating $operand1 $operator $operand2")
    }
  }

  def calculatorByMaps(operand1: String, operator: String, operand2: String): Unit = {
    def readInt(str: String): Option[Int] = if (str matches "\\d+") Some(str.toInt) else None

    def performOperator(a: Int, b: Int): Option[Int] = {
      operator match {
        case "+" => Some(a + b)
        case "-" => Some(a - b)
        case "/" => divide(a, b)
        case "*" => Some(a * b)
        case _ => None
      }
    }

    val result = readInt(operand1).flatMap { a =>
      readInt(operand2).flatMap { b =>
        performOperator(a, b)
      }
    }

    result match {
      case Some(number) => println(s"The answer is $number!")
      case None => println(s"Error calculating $operand1 $operator $operand2")
    }
  }

}
