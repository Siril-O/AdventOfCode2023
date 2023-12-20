package com.siril.advent.tasks.twenty

import com.siril.advent.tasks.Task
import com.siril.advent.tasks.eigth.Utils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object SignalsProcessor extends Task[Map[String, Element], Long] {

  override def solve(input: Map[String, Element]): Long = {
    val elements = Map("button" -> Button(Array("broadcaster"))) ++ input
    val initialMove = ("button", "broadcaster", false)
    val allMoves = ArrayBuffer[(String, String, Boolean)](initialMove)
    val queue = mutable.Queue[(String, String, Boolean)](initialMove)

    var buttonPushes = 1
    var finished = false
    while (!finished) {
      if (queue.isEmpty) {
        if (buttonPushes == 1000) finished = true else {
          allMoves.addOne(initialMove)
          queue.enqueue(initialMove)
          buttonPushes = buttonPushes + 1
          println(s"buttonPushes: $buttonPushes")
        }
      } else {
        val (prev, currId, signal) = queue.dequeue()
        if (elements.contains(currId)) {
          val processed = elements(currId).processSignal(signal, prev)
          val next = processed.map(v => (currId, v._1, v._2))
          allMoves.addAll(next)
          queue.enqueueAll(next)
        }
      }
    }
    print(allMoves.map(v => s"${v._1} -${if (v._3) "high" else "low"}> ${v._2}").mkString("\n"))
    val (high, low) = allMoves.partition(_._3)
    high.size.toLong * low.size.toLong
  }

  private def calcState(input: Map[String, Element]): Iterable[(String, Any)] =
    input.values.collect {
      case v@Conjunction(id, output, inputs) => id -> v.rememberedImpulses.toMap
      case v@Flip(id, output) => id -> v.on
    }

  override def solveAdvanced(input: Map[String, Element]): Long = {
    val elements = Map("button" -> Button(Array("broadcaster"))) ++ input
    val initialMove = ("button", "broadcaster", false)
    val queue = mutable.Queue[(String, String, Boolean)](initialMove)

    val ends = Set("rz", "mr", "jg", "kv")
    val subGraphs = mutable.Map.empty[String, Long]

    var buttonPushes = 1L
    var finished = false
    while (!finished) {
      if (queue.isEmpty) {
        queue.enqueue(initialMove)
        buttonPushes = buttonPushes + 1
      } else {
        val (prev, currId, signal) = queue.dequeue()
        if (elements.contains(currId)) {
          if (ends.contains(currId) && !signal && !subGraphs.contains(currId)) {
            subGraphs.addOne(currId -> buttonPushes)
            println(s"$currId: $buttonPushes")
            if (subGraphs.size == ends.size) finished = true
          }
          val processed = elements(currId).processSignal(signal, prev)
          queue.enqueueAll(processed.map(v => (currId, v._1, v._2)))
        }
      }
    }
    Utils.calcLCMFast(subGraphs.values.toArray)
  }

  private val regexp = "([%,&])([a-z]+)".r

  override def parseInput(raw: List[String]): Map[String, Element] = {
    val allElements: Map[String, Element] = raw.map { l =>
      l.split("->").toList match {
        case List(label, t) =>
          val targets = t.split(",").map(_.trim)
          label.trim match {
            case "broadcaster" => Broadcast(targets)
            case regexp("%", name) => Flip(name, targets)
            case regexp("&", name) => Conjunction(name, targets, Array.empty)
          }
      }
    }.map(e => e.id -> e).toMap
    allElements.collect {
      case (id, conjunction: Conjunction) => id -> conjunction.copy(inputs = allElements.values.filter(_.output.contains(id)).map(_.id).toArray)
      case v => v
    }
  }

  override def subDir: String = "twenty"
}

sealed trait Element {
  def id: String

  def output: Array[String]

  def processSignal(signal: Boolean, sender: String): Array[(String, Boolean)]
}

case class Flip(id: String, output: Array[String]) extends Element {
  var on = false

  def processSignal(signal: Boolean, sender: String): Array[(String, Boolean)] = {
    if (!signal) {
      val res = if (on) false else true
      on = !on
      output.map(_ -> res)
    } else Array.empty
  }
}

case class Broadcast(output: Array[String]) extends Element {
  val id: String = "broadcaster"

  override def processSignal(signal: Boolean, sender: String): Array[(String, Boolean)] =
    output.map(v => v -> signal)
}

case class Conjunction(id: String, output: Array[String], inputs: Array[String]) extends Element {

  val rememberedImpulses = mutable.Map[String, Boolean](inputs.map(_ -> false): _*)

  override def processSignal(signal: Boolean, sender: String): Array[(String, Boolean)] = {
    rememberedImpulses.update(sender, signal)
    val res = if (rememberedImpulses.values.forall(identity)) false else true
    output.map(_ -> res)
  }
}

case class Button(output: Array[String]) extends Element {
  val id: String = "button"

  override def processSignal(signal: Boolean, sender: String): Array[(String, Boolean)] =
    output.map(v => v -> signal)
}


