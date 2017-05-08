package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import rx.lang.scala.subjects.PublishSubject
import suggestions.observablex.ObservableEx


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should correctly use recovered") {
    val subject = PublishSubject[Int](0)

    var result: List[String] = Nil
    val obs = subject.recovered

    var completed = true
    obs.subscribe(x => {
      result = result ++ List(x.toString())
    }, e => assert(false), () => completed = true)

    subject.onNext(1)
    subject.onNext(2)
    subject.onNext(3)
    subject.onError(new Exception())

    assert(completed, "Should complete")

    //println(result)
    assert(result.length == 4)
  }

  test("WikipediaApi should correctly use timedOut") {
    val input = Observable.interval(0.9 second).take(10)

    var completed = false
    val obs = input.timedOut(2)
    obs.subscribe(_ => (), _ => (), () => completed = true)

    val result = obs.toBlockingObservable.toList

    assert(result == Seq(0, 1), "Sequences should equal")
    assert(completed, "Should complete")
  }

  test("Timeout test #2") {
    val input = Observable(1, 2, 3).zip(Observable.interval(700 millis)).timedOut(1L)
    val result = input.toBlockingObservable.toList
    assert(result.map(x => x._1) == Seq(1))
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) => {
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      } }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

  test("concatRecovered2") {
    val obs = Observable(1 to 5)
    val result = obs.concatRecovered(num => if (num != 4) Observable(num) else Observable(new Exception))

    val output = result.toBlockingObservable.toList
    assert(output(0).get == 1)
    assert(output(1).get == 2)
    assert(output(2).get == 3)
    assert(output(3).isFailure)
    assert(output(4).get == 5)
  }

//  test("concatRecovered2") {
//    val input = Observable("erik", "erik meijer2")
//    val output = input.concatRecovered(x => ObservableEx(wikipediaPage(x)))
//    var result = output.toBlockingObservable.toList
//
//    println(result)
//  }
}