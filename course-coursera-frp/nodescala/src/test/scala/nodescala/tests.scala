package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import javax.naming.directory.InvalidAttributeIdentifierException

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("'all' should succeed when all futures in the list succeed") {
    val input = (1 to 10).toList
    val futures = input.map(i => Future.always(i))

    val result = Await.result(Future.all(futures), 1 second)
    assert(result.sameElements(input))
  }

  test("'all' should fail when at least one future fails") {
    val error = new Exception()
    val futures = List(Future.always(5), Future.failed(error))

    try {
      Await.result(Future.all(futures), 1 second)
    } catch {
      case e: Throwable =>
        assert(error.equals(e))
    }
  }

  test("'all' should succeed when input is empty") {
    val result = Await.result(Future.all(Nil), 1 second)
    assert(result == Nil)
  }

  test("'any' should fail when input is empty") {
    try {
      Await.result(Future.any(Nil), 1 second)
      assert(false)
    } catch {
      case e: NoSuchElementException => //ok
    }
  }

  test("'any' should succeed when first completed future succeeds") {
    val failedFuture = for { f <- Future.delay(500 milliseconds); x <- Future.failed[Int](new Exception()) } yield x
    val succeededFuture = for { f <- Future.delay(100 milliseconds); x <- Future.successful(42) } yield x

    val input = List(failedFuture, succeededFuture)
    val result = Await.result(Future.any(input), 1 second)
    assert(result == 42)
  }

  test("'any' should fail when first completed future fails") {
    val error = new Exception()
    val failedFuture = for { f <- Future.delay(100 milliseconds); x <- Future.failed[Int](error) } yield x
    val succeededFuture = for { f <- Future.delay(500 milliseconds); x <- Future.successful(42) } yield x

    val input = List(failedFuture, succeededFuture)

    try {
      Await.result(Future.any(input), 1 second)
      assert(false)
    } catch {
      case e: Throwable =>
        assert(error.equals(e))
    }
  }

  test("Delayed future should be resolved after a delay") {
    val duration = 300 milliseconds
    val future = Future.delay(duration)

    Await.result(future, duration + 100.milliseconds)
  }

  test("Delayed future should fail before a delay") {
    val duration = 300 milliseconds
    val future = Future.delay(duration)

    try {
      Await.result(future, duration - 100.milliseconds)
      assert(false)
    } catch {
      case e: TimeoutException => //ok
    }
  }

  test("'continueWith' should work for succeeding futures") {
    val succeeded = Future(42)
    val continued = succeeded.continueWith(f => f.value.get.get.toString())

    val result = Await.result(continued, 1 second)
    assert(result == "42")
  }

  test("'continueWith' should work for failing futures") {
    val error = new Exception()
    val failed = Future.failed(error)
    val continued = failed.continueWith(f => f.value.get.get.toString())

    try {
      Await.result(continued, 1 second)
      assert(false)
    } catch {
      case e: Throwable => assert(error.equals(e))
    }
  }

  test("'continueWith' should return failed future when continuation for the successful future throws") {
    val error = new Exception()
    val f = Future(42)
    val continued = f.continueWith(f => throw error)

    try {
      Await.result(continued, 1 second)
      assert(false)
    } catch {
      case e: Throwable => assert(error.equals(e))
    }
  }

  test("'continue' should work for succeeding futures") {
    val succeeded = Future(42)
    val f = succeeded.continue(t => t.get.toString())

    val result = Await.result(f, 1 second)
    assert(result == "42")
  }

  test("'continue' should work for failing futures") {
    val error = new Exception()
    val failed = Future.failed(error)
    val continued = failed.continue(f => f.get.toString())

    try {
      Await.result(continued, 1 second)
      assert(false)
    } catch {
      case e: Throwable => assert(error.equals(e))
    }
  }

  test("'continue' should return failed future when continuation for the successful future throws") {
    val error = new Exception()
    val f = Future(42)
    val continued = f.continue(f => throw error)

    try {
      Await.result(continued, 1 second)
      assert(false)
    } catch {
      case e: Throwable => assert(error.equals(e))
    }
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




