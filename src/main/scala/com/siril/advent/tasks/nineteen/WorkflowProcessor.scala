package com.siril.advent.tasks.nineteen

import com.siril.advent.tasks.Task

import scala.collection.mutable.ArrayBuffer
object WorkflowProcessor extends Task[Workflows, Long] {

  type Combos = Map[String, (Long, Long)]
  private val initialFlow = "in"

  override def solveAdvanced(input: Workflows): Long = {
    val max = 4000L
    val initialCombos = Map("x" -> (0L, max), "m" -> (0L, max), "a" -> (0L, max), "s" -> (0L, max))
    val total = ArrayBuffer.empty[Combos]
    val stack = scala.collection.mutable.Stack[(String, Combos)](initialFlow -> initialCombos)
    while (stack.nonEmpty) {
      val curr = stack.pop()
      println(s"${curr._1}: ${curr._2.values.map(r => r._2 - r._1).product} ${curr._2.mkString}")
      val parts = input.workflows(curr._1)
      var left = curr._2
      for (part <- parts) {
        part.condition match {
          case Some(cond) => part.rule match {
            case Accepted => total.addOne(left.updated(cond.label, calcNewBoundary(cond, left(cond.label))))
              left = left.updated(cond.label, calcRestOfBoundary(cond, left(cond.label)))
            case Rejected =>
              left = left.updated(cond.label, calcRestOfBoundary(cond, left(cond.label)))
            case Redirect(toId) =>
              val old = left(cond.label)
              stack.push(toId -> left.updated(cond.label, calcNewBoundary(cond, old)))
              left = left.updated(cond.label, calcRestOfBoundary(cond, old))
          }
          case None => part.rule match {
            case Accepted => total.addOne(left)
            case Rejected => ()
            case Redirect(toId) => stack.push(toId -> left)
          }
        }
      }
    }
    total.map(v => v.values.map(r => r._2 - r._1).product).sum
  }

  private def calcRestOfBoundary(cond: Condition, old: (Long, Long)) = {
    if (cond.more)
      (old._1, Math.min(old._2, cond.value.toLong))
    else
      (Math.max(cond.value - 1L, old._1), old._2)
  }

  private def calcNewBoundary(cond: Condition, old: (Long, Long)) =
    if (cond.more)
      (Math.max(old._1, cond.value), old._2)
    else
      (old._1, Math.min(old._2, cond.value - 1))

  private def processMapping(m: Map[String, Int], workflows: Map[String, Array[Part]]): (Rule, Array[String]) = {
    var result = Option.empty[Rule]
    val path = ArrayBuffer("in")
    val stack = scala.collection.mutable.Stack.empty[String]
    stack.push("in")

    while (stack.nonEmpty) {
      var continue = true
      for (p <- workflows(stack.pop) if continue) {
        val evaluatedCond = p.condition match {
          case Some(c) if c.f(m(c.label)) => Some(p.rule)
          case Some(_) => None
          case None => Some(p.rule)
        }
        evaluatedCond.foreach { v =>
          v match {
            case Accepted => result = Some(Accepted)
            case Rejected => result = Some(Rejected)
            case Redirect(toId) =>
              path.addOne(toId)
              stack.push(toId)
          }
          continue = false
        }
      }
    }
    result.getOrElse(Rejected) -> path.toArray
  }

  override def solve(input: Workflows): Long = {
    val processed = input.mappings
      .map(m => m -> processMapping(m, input.workflows))

    processed.filter(_._2._1 == Accepted)
      .map(_._1.values.sum)
      .sum
  }

  private val wfRegexp = "([a-z]+)\\{(.+)}".r
  private val partReg = "(([a-z])([><])(\\d+):)?([a-zA-Z]+)".r

  override def parseInput(raw: List[String]): Workflows = {
    val wfs = raw.takeWhile(l => !l.isBlank)
    val workflows = wfs.map {
      case wfRegexp(id, p) =>
        val parts = p.split(",").map {
          case partReg(cond, label, operation, value, redirect) if Option(cond).isDefined =>
            Part(
              Some(Condition(label, operation == ">", value.toInt)), Rule.fromString(redirect)
            )
          case partReg(_, _, _, _, redirect) => Part(None, Rule.fromString(redirect))
        }
        (id, parts)
    }

    val mappings = raw.drop(wfs.length + 1).map { l =>
      val values = l.slice(1, l.length - 1).split(",")
      values.map(_.split("=").toList match {
        case List(name, value) => name -> value.toInt
      }).toMap
    }.toArray

    Workflows(workflows.toMap, mappings)
  }

  override def subDir: String = "nineteen"
}


case class Workflows(workflows: Map[String, Array[Part]], mappings: Array[Map[String, Int]])

case class Part(condition: Option[Condition], rule: Rule)

case class Condition(label: String, more: Boolean, value: Int) {
  val f: Int => Boolean = i => if (more) i > value else i < value
}

sealed trait Rule

object Accepted extends Rule

object Rejected extends Rule

case class Redirect(toId: String) extends Rule

object Rule {
  def fromString(str: String): Rule = str match {
    case "A" => Accepted
    case "R" => Rejected
    case id => Redirect(id)
  }
}