package com.test.monix


import java.time.LocalTime
import java.time.format.DateTimeFormatter

import monix.eval.Task
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.util.{Failure, Random, Success, Try}

class TaskTest extends FunSuite with Matchers {

  test("basic task: runAsync") {

    def callBack(result: Either[Throwable, Int]): Unit = result match {
      case Right(value) => println(value)
      case Left(ex) => println(ex.getClass())
    }

    val sum = Task {
      1 + 1
    }
    import monix.execution.Scheduler.Implicits.global

    sum.runAsync(callBack)

  }

  test("run foreach") {
    import monix.execution.Scheduler.Implicits.global

    val task = Task(1 + 1)
    val f = task.foreach(x => println(x))
  }

  test("Task now - evaluate right away") {
    val task = Task.now {
      println("Effect"); "Hello"
    }
    // "Effect" will be printed out
    val x = 1
  }

  test("Task eval") {
    import monix.execution.Scheduler.Implicits.global

    val h = Task.eval {
      println("Eval Effect"); "Hello eval!"
    }
    h.runToFuture.foreach(println)
  }

  /**
    * evalOnce builder does memoization on the first run
    */
  test("Task eval Once") {
    import monix.execution.Scheduler.Implicits.global

    val task = Task.evalOnce {
      println("Eval Once Effect");
      "Hello!"
    }
    task.runToFuture.foreach(println)
    task.runToFuture.foreach(println)
  }

  test("Task defer") {
    import monix.execution.Scheduler.Implicits.global

    val t = Task.defer {
      Task.now {
        println("Defer Effect"); "Hello!"
      }
    }
    // nothing will be printed.
    t.runToFuture.foreach(println)
    t.runToFuture.foreach(println)
  }

  test("Task from future") {
    import monix.execution.Scheduler.Implicits.global

    val t = Task.defer {
      val future = Future {
        println("Future print"); "Hello Future"
      }
      Task.fromFuture(future)
    }
    // nothing will be printed up to here
    t.runToFuture.foreach(println)

    val t2 = Task.fromFuture(Future {
      println("Future print2"); "Hello Future2"
    })
    // "Future print2" already printed
    t2.runToFuture.foreach(println)

    // defter future is a short cut for the code instance for t
    val t3 = Task.deferFuture(Future {
      println("Future print3"); "Hello Future3"
    })
    t3.runToFuture.foreach(println)

  }

  test("deferFutureAction") {
    // import monix.execution.Scheduler.Implicits.global

    def sumFuture(xs: List[Int])(implicit ec: ExecutionContext): Future[Int] = Future(xs.sum)

    //wrap a Future-API with an implicit execution context to a task
    def sumTask(xs: List[Int]) = Task.deferFutureAction {
      implicit scheduler => sumFuture(xs)
    }
  }

  /**
    * Task.fork ensures an asynchronous boundary, forcing the fork of a (logical) thread on execution.
    */
  test("Execute on customized thread pool") {

    // The default scheduler

    // Creating a special scheduler meant for I/O
    import monix.execution.Scheduler
    import monix.execution.Scheduler.Implicits.global
    lazy val io = Scheduler.io(name = "my-io")

    val source = Task(println(s"Running on thread: ${Thread.currentThread.getName}"))
    val forked = source.executeOn(io)
    forked.runToFuture.foreach(println)

    // This will run on the default thread pool
    val t2 = Task {
      println(s"Running on thread: ${Thread.currentThread.getName}")
    }
    t2.runToFuture

  }

  test("RaiseError lifts errors to task monadic context") {
    val error = Task.raiseError[Int](new TimeoutException("unable to proceed"))

  }

  test("Never - returns a task instance that never completes") {
  }

  test("Task.unit - Return  an already completed Task[Unit] instance") {
    val unit = Task.unit
  }

  test("Memoization-- it applies the memoization on the first runToFuture") {
    val task = Task {
      println("Execute now"); "Hello memoization"
    }
    val memoized = task.memoize

    import monix.execution.Scheduler.Implicits.global
    memoized.runToFuture.foreach(println)
    memoized.runToFuture.foreach(println)
  }

  test("Memoize success only, for failures , it will keep trying until a success value is availabe") {
    var effect = 0
    val task = Task.eval {
      effect += 1
      if (effect < 3) throw new RuntimeException("dummy") else effect
    }
    val memoizedSuc = task.memoizeOnSuccess
    import monix.execution.Scheduler.Implicits.global
    val f1 = memoizedSuc.runToFuture
    val f2 = memoizedSuc.runToFuture
    val f3 = memoizedSuc.runToFuture
    val f4 = memoizedSuc.runToFuture
    f3.map(_ shouldBe 3)
    f4.map(_ shouldBe 3)

  }

  /**
    * FlatMap Creates a new Task by applying a function to the successful result
    * * of the source Task, and returns a task equivalent to the result
    * * of the function.
    */
  test("flatmap") {
    def fib(n: BigInt, a: BigInt, b: BigInt): Task[BigInt] = {
      Task.eval(n > 0).flatMap {
        case false => Task.now(b)
        case true => fib(n - 1, b, a + b)
      }
    }
    import monix.execution.Scheduler.Implicits.global
    fib(5, 0, 1).runToFuture.map(_ shouldBe 8)
  }

  test("parZip/Parmap") {
    val locationTask = Task.eval("location")
    val phoneTask = Task.eval("phone")
    val addressTask = Task.eval("address")

    //Potentially executed in parallel
    val result = Task.parZip3(locationTask, phoneTask, addressTask).map {
      case (location, phone, address) => s"Got location:$location,phone:$phone,address:$address"
    }

    val expected = "Got location:location,phone:phone,address:address"
    import monix.execution.Scheduler.Implicits.global
    result.runToFuture.map(_ shouldBe (expected))

    // parMap
    val result2 = Task.parMap3(locationTask, phoneTask, addressTask) {
      (location, phone, address) => s"Got location:$location,phone:$phone,address:$address"
    }
    result2.runToFuture.map(_ shouldBe expected)

  }

  class TaskOps[A](t:Task[A]){
    def retryMaxTimeOrPredicated(maxTime: FiniteDuration, predicate: A => Boolean, interval: FiniteDuration): Task[A] = {
      t.flatMap { a =>
        if (predicate(a)) Task.now(a)
        else if (maxTime.length <= 0 ) Task.raiseError(new RuntimeException("Failed after retries"))
        else retryMaxTimeOrPredicated(maxTime - interval, predicate, interval).delayExecution(interval)
      }
    }

  }

  test("Task.sequence -- convert a Seq[Task[A]] to Task[Seq[A]], the order is guaranteed, tasks get evaluated sequentially") {
    val t1 = Task {
      println("task1"); "t1"
    }
    val t2 = Task {
      println("task2"); "t2"
    }
    val taskSeq = Task.sequence(Seq(t1, t2))
    import monix.execution.Scheduler.Implicits.global
    taskSeq.runToFuture.map(resultSeq => resultSeq shouldBe (Seq("t1", "t2")))
  }


  test("Task.gather -- A nondeterministic version of Task.sequence, result is ordered, but effects are NOT potentially exectued in parallel") {
    val t1 = Task {
      println("task1"); "t1"
    }
    val t2 = Task {
      println("task2"); "t2"
    }
    val taskSeq = Task.gather(Seq(t1, t2))
    import monix.execution.Scheduler.Implicits.global
    taskSeq.runToFuture.map(resultSeq => resultSeq shouldBe (Seq("t1", "t2")))

  }

  test("Task.gatherUnordered -- A nondeterministic version of Task.sequence, result order is nondeterministic, effects are NOT, potentially exectued in parallel") {
    val t1 = Task {
      println("task1"); "t1"
    }
    val t2 = Task {
      println("task2"); "t2"
    }
    val taskSeq = Task.gatherUnordered(Seq(t1, t2))
    import monix.execution.Scheduler.Implicits.global

    import scala.math.Ordering._
    taskSeq.runToFuture.map(resultSeq => resultSeq.sorted shouldBe (Seq("t1", "t2")))
  }

  test("Task.racePair/raceMany/race -- choose the winner between Tasks") {
    import monix.execution.Scheduler.Implicits.global

    import scala.concurrent.duration._
    val t1 = Task(1 + 1).delayExecution(1.second)
    val t2 = Task(1 + 10).delayExecution(1.second)
    val raceResult = Task.racePair(t1, t2).runToFuture
    raceResult.map {
      case Left((a, futureB)) =>
        futureB.cancel
        println(s"winner in racePair is a:$a")
      case Right((futureA, b)) =>
        futureA.cancel
        println(s"Winner in racePair is b:$b")
    }

    // race Many
    val raceManyResult = Task.raceMany(Seq(t1, t2)).runToFuture
    raceManyResult.map { x => println(s"winner in raceMany is x:$x") }

    /* race will cancel the loser automatically */
    val raceListResult = Task.race(t1, t2).runToFuture
    raceListResult.map {
      case Left(a) =>

        println(s"winner in race is a:$a")
      case Right(b) =>
        println(s"Winner in race is b:$b")
    }
    Thread.sleep(3 * 1000)
  }


  test("DelayExecution") {
    val task = Task {
      println("Delayed task"); "Hello in a delayed task"
    }
    import monix.execution.Scheduler.Implicits.global

    import scala.concurrent.duration._
    val delayedTask = task.delayExecution(5.seconds)
    delayedTask.runToFuture.map(_ shouldBe "Hello in a delayed task")
    Thread.sleep(6 * 1000)
  }

  test("delayExecutionWith-- now flat map") {

    import monix.execution.Scheduler.Implicits.global
    val trigger = Task.unit.delayExecution(1.second)
    val task = Task {
      println("Delayed task"); "Hello in a delayed task"
    }
    val delayedTask = trigger.flatMap(_ => task)
    delayedTask.runToFuture.map(_ shouldBe "Hello in a delayed task")
    Thread.sleep(6 * 1000)
  }

  def prtCurTime(msg: String = "ß"): Unit = println(msg + ":" + LocalTime.now().format(DateTimeFormatter.ISO_LOCAL_TIME))

  test("Task.delayResult delays the signaling of the result, but not the execution of the Task") {
    import monix.execution.Scheduler.Implicits.global
    val task = Task {
      prtCurTime("startEvaluation");
      prtCurTime("Delayed execution");
      "Hello"
    }
    val delayed = task.delayExecution(1.second).delayResult(5.second)
    delayed.runToFuture.map { s =>
      prtCurTime(s"Result:$s")
    }

    // delayResultBySelector ,now favors of flatMap
    //task.delayResultBySelector()
   //
    Thread.sleep(8 * 1000)
  }

  test("Task.doOnFinish executes the supplied Option[Throwable] => Task[Unit] function when the source finishes, being meant for cleaning up resources or executing some scheduled side-effect"){
    import monix.execution.Scheduler.Implicits.global
    val task = Task(1)
    val withDoFinish = task.doOnFinish{
      case None => println("Was a success");Task.unit
      case Some(ex) => println(s"Failed:$ex");Task.unit
    }
    withDoFinish.runToFuture.foreach(x=>println(x))
  }

  test("Customized error logging") {
    val reporter = UncaughtExceptionReporter { ex =>
      println(s"my error report:${ex.toString}")
    }
    import monix.execution.Scheduler.{global => default}
    implicit val myScheculer: Scheduler = Scheduler(default, reporter)

    val task = Task {
      1
    }
    val delayed = task.delayExecution(10.seconds).timeout(3.seconds).executeOn(myScheculer, true)

    delayed.runToFuture.map(x => x shouldBe (2))
    delayed.runAsync{r =>
        prtCurTime("result");
        println(r)
   }
    Thread.sleep(5*1000)
  }

  test("Task.onErrorHandleWith is an operation that takes a function mapping possible exceptions to a desired fallback outcome"){
    val task = Task {"Hello from onErrorHandleWith"}.delayExecution(10.second).timeout(3.seconds)
    val recovered = task.onErrorHandleWith{
      case _:TimeoutException => Task.now("Recovered task ran")
      case other => Task.raiseError(other)
    }
    import monix.execution.Scheduler.Implicits.global
    recovered.runToFuture.map(_ shouldBe("Revovered task ran"))
    recovered.runAsync{
      case Left(ex) => println("Exception ex:$ex")
      case Right(r) => prtCurTime(s"Result:$r")

    }
    Thread.sleep(5*1000)

  }

  test("Task.onErrorRecoverWith is an operation that takes a function mapping possible exceptions to a desired fallback outcome"){
    val task = Task {"Hello from onErrorRecoverWith"}.delayExecution(10.second).timeout(3.seconds)
    val recovered = task.onErrorRecoverWith{
      case _:TimeoutException => Task.now("Recovered task ran")
    }
    import monix.execution.Scheduler.Implicits.global
    recovered.runToFuture.map(_ shouldBe("Revovered task ran"))
    recovered.runAsync{
      case Left(ex) => println("Exception ex:$ex")
      case Right(r) => prtCurTime(s"Result:$r")

    }
    Thread.sleep(5*1000)

  }

  test("Task retry with exponential backoff"){
    def retryBackOff[A](task:Task[A],maxRetry:Int,delay:FiniteDuration):Task[A]={
      task.onErrorHandleWith{
        case ex:Exception => if(maxRetry > 0 ) retryBackOff(task,maxRetry - 1, delay * 2).delayExecution(delay )
                             else Task.raiseError(ex)
      }
    }

  }

  test("onErrorRestartIf/onErrorRestart(maxRetries) -- restart the task on errors "){
    val task = Task(Random.nextInt()).flatMap{
      case even if even % 2 == 0 => Task.now(even)
      case other => Task.raiseError(new IllegalArgumentException("invalid number:"+other))
    }
    val randomEven = task.onErrorRestartIf{
      case _:IllegalArgumentException =>true
      case _ => false
    }
    import monix.execution.Scheduler.Implicits.global
    randomEven.runToFuture.map(x=> println(s"Got an even:$x"))

    val randomEvenMaxTry = task.onErrorRestart(4)
    randomEvenMaxTry.runToFuture.map(x=> println(s"Got an even after retires:$x"))
  }

  test("Task materialize/dematerialize --Convert to Task[Try[A]] and back to Task[A]"){

    val errorTask = Task{ throw new IllegalStateException;2}
    val materialized:Task[Try[Int]] = errorTask.materialize

    import monix.execution.Scheduler.Implicits.global
    materialized.runToFuture.map{
      case Success(value) => println(s"Got result:$value"); false shouldBe true
      case Failure(ex) => println(s"Oops, exception:$ex")
    }


  }

  test("Task parMap2"){
    val fa1 = Task(1)
    val fa2 = Task(2)

       // Yields Success(3)
     val x =   Task.parMap2(fa1, fa2) { (a, b) =>
          a + b
     }
    import monix.execution.Scheduler.Implicits.global
    val y = x.runToFuture
    Thread.sleep(1000)
    y.map (_ shouldBe(3))

  }

}
