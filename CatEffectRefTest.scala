package com.test.effect


import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._
import cats.kernel.Eq
import org.scalatest.{AsyncFunSuite, Matchers, Succeeded}
import org.scalatest.compatible.Assertion

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class RefTests extends AsyncFunSuite with Matchers {

  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  implicit val timer: Timer[IO] = IO.timer(executionContext)
  implicit val cs: ContextShift[IO] = IO.contextShift(executionContext)

  private val smallDelay: IO[Unit] = timer.sleep(20.millis)

  private def awaitEqual[A: Eq](t: IO[A], success: A): IO[Unit] =
    t.flatMap(a => if (Eq[A].eqv(a, success)) IO.unit else smallDelay *> awaitEqual(t, success))

  private def run(t: IO[Unit]): Future[Assertion] = t.as(Succeeded).unsafeToFuture

  test("concurrent modifications") {
    val finalValue = 100
    val r = Ref.unsafe[IO, Int](0)
    val modifies = List.fill(finalValue)(IO.shift *> r.update(_ + 1)).sequence
    run(IO.shift *> modifies.start *> awaitEqual(r.get, finalValue))
  }

  test("getAndSet - successful") {
    val op = for {
      r <- Ref[IO].of(0)
      getAndSetResult <- r.getAndSet(1)
      getResult <- r.get
    } yield getAndSetResult == 0 && getResult == 1

    run(op.map(_ shouldBe true))
  }

  test("access - successful, return the value and setter") {
    val op = for {
      r <- Ref[IO].of(0)
      valueAndSetter <- r.access
      (value, setter) = valueAndSetter
      success <- setter(value + 1)
      result <- r.get
    } yield success && result == 1
    run(op.map(_ shouldBe true))
  }

  test("access - setter should fail if value is modified before setter is called") {
    val op = for {
      r <- Ref[IO].of(0)
      valueAndSetter <- r.access
      (value, setter) = valueAndSetter
      _ <- r.set(5)
      success <- setter(value + 1)
      result <- r.get
    } yield !success && result == 5
    run(op.map(_ shouldBe true))
  }

  test("access - setter should fail if called twice") {
    val op = for {
      r <- Ref[IO].of(0)
      valueAndSetter <- r.access
      (value, setter) = valueAndSetter
      cond1 <- setter(value + 1)
      _ <- r.set(value)
      cond2 <- setter(value + 1)
      result <- r.get
    } yield cond1 && !cond2 && result == 0
    run(op.map(_ shouldBe true))
  }
}
