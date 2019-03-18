package com.test.shapeless

import org.scalatest.{FunSuite, Matchers}
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

sealed trait JsonValue
case class JsonObject(fields:List[(String,JsonValue)]) extends JsonValue
case class JsonArray(array:List[JsonValue]) extends JsonValue
case class JsonString(s:String) extends JsonValue
case class JsonNumber(d:Double) extends JsonValue
case class JsonBoolean(b:Boolean) extends JsonValue
case object JsonNull extends JsonValue

// A type class json encoder
trait JsonEncoder[A]{
  def encode(a:A):JsonValue
}

object JsonEncoder{
  def apply[A](implicit encoder: JsonEncoder[A]): JsonEncoder[A] = encoder

  // Helper method to create a json encoder
  def createJsonEncoder[A](f: A => JsonValue): JsonEncoder[A] = new JsonEncoder[A] {
    override def encode(a: A): JsonValue = f(a)
  }

  //implicit  instances of primitive encoders
  implicit val strEncoder:JsonEncoder[String] = createJsonEncoder(s => JsonString(s))
  implicit val numEncoder:JsonEncoder[Double] = createJsonEncoder(s => JsonNumber(s))
  implicit val intEncoder:JsonEncoder[Int] = createJsonEncoder(s => JsonNumber(s))
  implicit val longEncoder:JsonEncoder[Long] = createJsonEncoder(s => JsonNumber(s))
  implicit val floatEncoder:JsonEncoder[Float] = createJsonEncoder(s => JsonNumber(s))
  implicit val byteEncoder:JsonEncoder[Byte] = createJsonEncoder(s => JsonNumber(s))
  implicit val boolEncoder:JsonEncoder[Boolean] = createJsonEncoder(s => JsonBoolean(s))

  // a few instance combinators:
  implicit def listEncoders[A](implicit enc:JsonEncoder[A]):JsonEncoder[List[A]] =
    createJsonEncoder[List[A]](list => JsonArray(list.map(enc.encode)))

  // option
  implicit def optionEncoders[A](implicit enc:JsonEncoder[A]):JsonEncoder[Option[A]] =
    createJsonEncoder[Option[A]](opt => opt.map(enc.encode).getOrElse(JsonNull))

}


trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(a:A):JsonObject
}

object JsonObjectEncoder{

  def apply[A](implicit enc:JsonObjectEncoder[A]) = enc
  def createObjectEncoder[A](f: A => JsonObject):JsonObjectEncoder[A] = new JsonObjectEncoder[A]{
    override def encode(a: A): JsonObject = f(a)
  }

  implicit val hnilEncoder:JsonObjectEncoder[HNil] = createObjectEncoder[HNil](nhil => JsonObject(Nil))

  implicit def hListEncoder[K <: Symbol,H,T <:HList](implicit witness: Witness.Aux[K], hEncoder:Lazy[JsonEncoder[H]],tEncoder: JsonObjectEncoder[T]):JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    createObjectEncoder[FieldType[K, H] :: T]{ hList =>
      val h = hEncoder.value.encode(hList.head)
      val tail = tEncoder.encode(hList.tail)
      JsonObject( (fieldName,h) :: tail.fields)
    }

  }

  implicit def genericObjectEncoder[A, H](implicit generic: LabelledGeneric.Aux[A,H],hEncoder:Lazy[JsonObjectEncoder[H]]):JsonObjectEncoder[A] ={
    createObjectEncoder[A]( a=> {
      val g = generic.to(a)
      hEncoder.value.encode(g)
    }
    )
  }

}


class LabelledGenericTest extends FunSuite with Matchers {

  test("JsonEncoder"){

    import shapeless.syntax._
    import shapeless.LabelledGeneric._
    val iceCream = IceCream("ice1",2,true)
    val gen = JsonObjectEncoder[IceCream].encode(iceCream)
    println(gen)



  }

}
