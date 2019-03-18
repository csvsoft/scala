package com.test.shapeless

import org.scalatest.{FunSuite, Matchers}
import shapeless._

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

class ShapelessTest extends FunSuite with Matchers {

  case class Employee(id: String, age: Int, manager: Boolean)

  case class Icecream(name: String, numCherries: Int, inCone: Boolean)

  test("Encode product") {

    val employee = Employee("Davis", 32, false)
    // To generic representation
    val genericEmployee = Generic[Employee].to(Employee("Davis", 32, false))

    genericEmployee shouldBe ("Davis" :: 32 :: false :: HNil)

    val genericIceCream = Generic[Icecream].to(Icecream("Sundae", 2, false))
    genericIceCream shouldBe ("Sundae" :: 2 :: false :: HNil)


    //from
    val employeeBack = Generic[Employee].from(genericEmployee)
    employeeBack shouldBe (employee)


  }

  test("Co product"){
    sealed trait Shape
    final case class Rectangle(width:Double, length:Double) extends Shape
    final case class Circle(radius:Double) extends Shape


    val genShape = Generic[Shape]
    val recRep = genShape.to(Rectangle(2,3))
    val circleRep = genShape.to(Circle(2))
    recRep shouldBe (Inl(Rectangle(2,3)))
    circleRep shouldBe()

  }

  test("coproduct - light"){
    case class Red()
    case class Green()
    case class Amber()
    type Light = Red :+: Green :+: Amber :+: CNil
    // :+: Either with Subtype InL and InR
    val green = Inl(Green())

  }

  test("LabelledGeneric"){
    import shapeless._
    import shapeless.{HList, ::, HNil}
    import shapeless.syntax.singleton._

    case class Cat(name:String,orange:Boolean)
    val garField = ("name" ->> "Garfield") :: ("orange" ->> true) :: HNil

    //val garFieldObj = LabelledGeneric[Cat].from(garField)


    val iceCream = IceCream("Sundae", 1, false)
    val x = LabelledGeneric[IceCream].to(iceCream)
    val y = x


  }

}
