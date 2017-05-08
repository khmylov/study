/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection. */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  var pendingQueue = Queue.empty[Operation]

  def receive = normal

  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {

    case op: Operation => root ! op

    case GC =>
      val nr = createRoot
      root ! CopyTo(nr)
      context.become(garbageCollecting(nr), false)
  }

  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {

    case CopyFinished =>
      root ! PoisonPill
      root = newRoot
      context.unbecome
      pendingQueue.foreach(x => root ! x)
      pendingQueue = Queue.empty

    case op: Operation => pendingQueue = pendingQueue.enqueue(op) // stash

    // http://doc.akka.io/docs/akka/snapshot/scala/actors.html
    // ...need to provide a pattern match for all messages that it can accept
    // and if you want to be able to handle unknown messages then you need to
    // have a default case... Otherwise an
    // akka.actor.UnhandledMessage(message, sender, recipient) will be
    // published to the ActorSystem's EventStream.
    case GC =>
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean = false) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  var expectedFinishMessageCount = 0

  def receive = normal

  def findSubNode(element: Int) =
    subtrees.get(if (element < elem) Left else Right)


  val normal: Receive = {

    case contains @ Contains(source, id, element) =>
      if (element == elem) {
        source ! ContainsResult(id, !removed)
      } else {
        findSubNode(element) match {
          case Some(x) => x ! contains
          case None => source ! ContainsResult(id, false)
        }
      }

    case insert @ Insert(source, id, element) =>
      if (element == elem) {
        removed = false
        source ! OperationFinished(id)
      } else {
        val direction = if (element < elem) Left else Right
        findSubNode(element) match {
          case Some(x) => x ! insert
          case None =>
            subtrees = subtrees + ( direction -> context.actorOf(props(element)) )
            source ! OperationFinished(id)
        }
      }

    case remove @ Remove(source, id, element) =>
      if (element == elem) {
        removed = true
        source ! OperationFinished(id)
      } else {
        findSubNode(element) match {
          case Some(x) => x ! remove
          case None => source ! OperationFinished(id)
        }
      }

    case copy @ CopyTo(target) =>
      expectedFinishMessageCount = 0

      if (!removed) {
        target ! Insert(self, elem, elem)
        expectedFinishMessageCount = expectedFinishMessageCount + 1
      }

      if (subtrees.nonEmpty) {
        subtrees.foreach { case (_, x) => x ! copy }
        expectedFinishMessageCount = expectedFinishMessageCount + subtrees.size
      }

      if (expectedFinishMessageCount == 0) {
        context.parent ! CopyFinished
      }

    case CopyFinished =>
      expectedFinishMessageCount = expectedFinishMessageCount - 1
      if (expectedFinishMessageCount == 0) {
        context.parent ! CopyFinished
      }

    case OperationFinished(id) =>
      if (id == elem) {
        self ! CopyFinished
      } else {
        throw new IllegalStateException("Got unexpected $op but the node elem = $elem")
      }

  }
}
