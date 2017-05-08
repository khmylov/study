package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("When inserting two numbers, findMin should return the minimum of the inserted values") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("Inserting an element into an empty heap, then deleting the minimum should return an empty heap") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("Minimum of two melded heaps should be the minimum among minimums of each heap") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    findMin(h) == Math.min(findMin(h1), findMin(h2))
  }

  property("Finding and deleting minimum values should produce a sorted sequence") = forAll { h: H =>
    def isOrdered(h: H, currentMin: Int): Boolean = {
      if (isEmpty(h)) true
      else {
        val v = findMin(h)
        if (v < currentMin) false
        else isOrdered(deleteMin(h), v)
      }
    }

    isOrdered(h, Int.MinValue)
  }

  property("Inserting the successor of the minimum then deleting the minimum reports the successor as the new minimum") = forAll { h: H =>
    val m = findMin(h)
    findMin(deleteMin(insert(m + 1, h))) == m + 1
  }

  def printHeap(h: H) = {
    def loopPrint(h1: H): Unit = {
      if (!isEmpty(h1)) {
        val v = findMin(h1)
        System.console.printf("%s ", v.toString())
        loopPrint(deleteMin(h1))
      } else {
        System.console.printf("\n")
      }
    }

    loopPrint(h)
  }

  lazy val genHeap: Gen[H] = for {
    a <- Gen.choose(Int.MinValue + 1, Int.MaxValue - 1)
    h <- oneOf(value(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
