package com.test.cats

import cats.ApplicativeError
import org.scalatest.{FunSuite, Matchers}

import scala.reflect.runtime.universe._
import cats.implicits._
class SyntaxExention extends FunSuite with Matchers{

  test("extension mesthods"){


    //
    val aOpiton:Option[String] ="a".some
    val aRight:Either[Int,String] = "a".asRight[Int]

    val success1 = "a".asRight[Int]
    val success2 = "b".asRight[Int]

    val r2 = success1 *> success2
    r2 shouldBe(Right("b"))
  }

  test("ApplicativeError"){

    def divide[F[_]](dividend: Int, divisor: Int)(implicit F: ApplicativeError[F, String]): F[Int] =
       if (divisor == 0) F.raiseError("division by zero")
       else F.pure(dividend / divisor)
    type ErrorOr[A] = Either[String, A]
    type ErrorTry[A] = scala.util.Try[A]
    val r = divide[ErrorOr](2,1)
    r shouldBe(Right(2))
    divide[ErrorOr](2,0) shouldBe(Left("division by zero"))


  }

}
