package simulations

import math.random

case class Room(row: Int, column: Int)

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val maxMoveDelay: Int = 5

    val prevalenceRate: Double = 0.01

    val transmissibilityRate: Double = 0.4
    val incubationTime: Int = 6
    val immunityTime: Int = 16
    val healingTime: Int = 18
    val deathTime: Int = 14
    val deathChance: Double = 0.25
  }

  import SimConfig._

  val persons: List[Person] = {
    val totalInfected = population * prevalenceRate
    ((1 to population) map (id => {
      val person = new Person(id)
      if (id <= totalInfected) {
        person.makeInfected()
      }

      person.init()
      person
    })).toList
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    def visiblyInfected = sick || dead

    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def init() {
      def moveAction(): Unit = {
        val moveInDay = randomBelow(maxMoveDelay) + 1
        afterDelay(moveInDay) {
          if (!dead) {
            val neighbor = getNotInfectedRoom(getNeighbors(Room(row, col)))
            neighbor match {
              case Some(x) => moveTo(x)
              case None => // do nothing
            }

            this.schedule(moveAction)
          }
        }
      }

      this.schedule(moveAction)
    }

    def schedule(action: Action) {
      action()
    }

    def isInRoom(room: Room) = row == room.row && col == room.column

    def moveTo(room: Room) {
      row = room.row
      col = room.column

      if (!infected && random <= transmissibilityRate) {
        makeInfected()
      }
    }

    def makeInfected() {
      def infectedAction(): Unit = {
        infected = true

        afterDelay(incubationTime) { sick = true }

        afterDelay(deathTime) {
          if (random <= deathChance) {
            dead = true
          }
        }

        afterDelay(immunityTime) {
          if (!dead) {
            immune = true
            sick = false
          }
        }

        afterDelay(healingTime) {
          if (!dead) {
            heal()
          }
        }
      }

      schedule(infectedAction)
    }

    def heal() = {
      infected = false
      sick = false
      dead = false
      immune = false
    }
  }

  def isRoomInfected(room: Room) = persons.exists(p => p.isInRoom(room) && p.visiblyInfected)

  def getNotInfectedRoom(rooms: List[Room]): Option[Room] = {
    rooms match {
      case Nil => None
      case _ =>
        val room = rooms((random * rooms.length).toInt)
        if (!isRoomInfected(room)) {
          Some(room)
        } else {
          getNotInfectedRoom(rooms.diff(List(room)))
        }
    }
  }

  def getNeighbors(room: Room): List[Room] = {
    val left = (room.column - 1 + roomRows) % roomRows
    val right = (room.column + 1) % roomRows
    val top = (room.row - 1 + roomColumns) % roomColumns
    val bottom = (room.row + 1) % roomColumns
    List(Room(room.row, left), Room(room.row, right), Room(top, room.column), Room(bottom, room.column))
  }
}
