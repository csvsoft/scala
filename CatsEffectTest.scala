package com.test.effect

import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.Future
import cats.Parallel
import cats.effect.{Async, ConcurrentEffect}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds
import cats.implicits._

class ParalellTest extends FunSuite with Matchers{

  class ParallelExecIO[F[_]: ConcurrentEffect, G[_]](implicit ev: Parallel[F, G]) {

    def exec: F[List[String]] = {
      val foo = Async.shift[F](implicitly) *> "foo".pure[F]
      val bar = Async.shift[F](implicitly) *> "bar".pure[F]
      List(foo, bar).parSequence
    }
  }

  test("paralell"){
    case class UserProfile(user:String,profile:String)
    import cats.syntax.applicative._
    //(getUserFuture(),getUserProfile()).map2()



  }

  def getUserFuture():Future[String] = Future("user")
  def getUserProfile():Future[String] = Future("userProfile")


}
