package com.test.shapeless

import org.scalatest.{FunSuite, Matchers}
import shapeless.{::, Generic, HList, HNil}

trait CSVEncoder[A] {
  def encode(a: A): List[String]
}

object CSVEncoder {

  // Sommoner
  def apply[A](implicit enc: CSVEncoder[A]): CSVEncoder[A] = enc

  // constructor
  def instance[A](f: A => List[String]): CSVEncoder[A] = new CSVEncoder[A] {
    override def encode(a: A): List[String] = f(a)
  }

  // globally visible instance
  implicit val strEncoder: CSVEncoder[String] = instance(s => List(s))
  implicit val intEncoder: CSVEncoder[Int] = instance(s => List(String.valueOf(s)))
  implicit val boolEncoder: CSVEncoder[Boolean] = instance(s => List(String.valueOf(s)))
  implicit val hnilEncoder: CSVEncoder[HNil] = instance(hnil => Nil)

  implicit def hListEncoder[H, T <: HList](implicit hEncoder: CSVEncoder[H], tailEncoder: CSVEncoder[T]): CSVEncoder[H :: T] = {
    instance[H :: T] { case h :: t =>
      hEncoder.encode(h) ++ tailEncoder.encode(t)
    }
  }


  implicit def genericEncoder[A, R](implicit generic: Generic[A] {type Repr = R}, enc: CSVEncoder[R]): CSVEncoder[A] = {
    instance[A] { a =>
      enc.encode(generic.to(a))
    }
  }
}


class CSVEncoderTest extends FunSuite with Matchers {

  test("CSVEncoder") {
    val iceCream = IceCream("ice1", 2, true)
    val csvResult = CSVEncoder[IceCream].encode(iceCream)

    println(csvResult)

  }

}
