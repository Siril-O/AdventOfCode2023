package com.siril.advent.tasks.eighteen

import com.siril.advent.tasks.Task
import com.siril.advent.tasks.seventeen._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DigMap extends Task[Array[DigItem], Long] {
  val extension = 10

  def initShape(input: Array[DigItem]) = {
    val arr = ArrayBuffer(new ArrayBuffer[Boolean]())
    var current = (0, 0)
    input.foreach { i =>
      for (_ <- 0 until i.steps) {
        current = Point.move(current, i.dir)
        if (current._1 > arr.length - 1) addRow(arr, before = false)
        if (current._1 < 0) {
          addRow(arr, before = true)
          current = (current._1 + extension, current._2)
        }
        if (current._2 > arr(current._1).length - 1) addColl(arr, before = false)
        if (current._2 < 0) {
          current = (current._1, current._2 + extension)
          addColl(arr, before = true)
        }
        arr(current._1).update(current._2, true)
      }
    }
    arr
  }


  def floodFill(arr: ArrayBuffer[ArrayBuffer[Boolean]], targetColor: Boolean, initialPos: (Int, Int) = 34 -> 127): Unit = {
    val visited = Array.fill(arr.length, arr.head.length)(false)

    def isValidCoord(pos: (Int, Int)) =
      pos._1 > 0 && pos._1 < arr.length && pos._2 > 0 && pos._2 < arr.head.length

    val queue = mutable.Queue[(Int, Int)](initialPos)
    while (queue.nonEmpty) {
      val (x, y) = queue.dequeue()
      arr(x)(y) = targetColor

      val coords = Array((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
      for ((nx, ny) <- coords) {
        if (isValidCoord(nx, ny) && !visited(nx)(ny) && arr(nx)(ny) != targetColor) {
          queue.enqueue((nx, ny))
          visited(nx)(ny) = true
        }
      }
    }
  }

  override def solve(input: Array[DigItem]): Long = {
    val arr = initShape(input)
    println(arr.map(_.map(v => if (v) "#" else ".").mkString).mkString("\n"))

    floodFill(arr, targetColor = true)

    println("Filled:")
    println(arr.map(_.map(v => if (v) "#" else ".").mkString).mkString("\n"))

    arr.map(_.count(identity)).sum
  }

  override def solveAdvanced(i: Array[DigItem]): Long = {
    val input = i.map(v => DigItem(v.color.last match {
      case '0' => Right
      case '1' => Down
      case '2' => Left
      case '3' => Up
    }, java.lang.Integer.parseInt(v.color.take(v.color.length - 1), 16), ""))

    val edges = computeEdges(input)
    shoelaceCalc(edges) + calcPerimeter(edges) / 2 + 1
  }

  def calcPerimeter(edges: Array[(Int, Int)]): Long = {
    var perimeter = 0L
    var prev = edges.head
    for ((x, y) <- edges) {
      perimeter = perimeter + Math.abs(x - prev._1) + Math.abs(y - prev._2)
      prev = (x, y)
    }
    perimeter
  }

  def shoelaceCalc(edges: Array[(Int, Int)]): Long = {
    def calcDeterminant(a: (Int, Int), b: (Int, Int)): Long = a._1.toLong * b._2.toLong - b._1.toLong * a._2.toLong

    var area = 0L
    var prev = edges.head
    for ((x, y) <- edges) {
      area = area + calcDeterminant(prev, (x, y))
      prev = (x, y)
    }
    Math.abs(area) / 2
  }

  def computeEdges(input: Array[DigItem]): Array[(Int, Int)] = {
    val edges = new ArrayBuffer[(Int, Int)]()
    var cur = 0 -> 0
    edges.addOne(cur)
    for (i <- input) {
      cur = Point.move(cur, i.dir, i.steps)
      edges.addOne(cur)
    }
    edges.toArray
  }


  def addRow(arr: ArrayBuffer[ArrayBuffer[Boolean]], before: Boolean, size: Int = extension): Unit = {
    val toAdd = ArrayBuffer.fill(size)(ArrayBuffer.fill(arr.head.length)(false))
    if (before)
      arr.prependAll(toAdd)
    else
      arr.addAll(toAdd)
  }

  def addColl(arr: ArrayBuffer[ArrayBuffer[Boolean]], before: Boolean, size: Int = extension): Unit =
    arr.indices.foreach(i =>
      if (before)
        arr(i).prependAll(Array.fill(size)(false))
      else
        arr(i).addAll(Array.fill(size)(false))
    )


  val regexp = "([A-Z])\\s+(\\d+)\\s+\\(#([a-zA-Z0-9]+)\\)".r

  override def parseInput(reg: List[String]): Array[DigItem] = {
    reg.filterNot(_.isBlank).map {
      case regexp(direction, s, color) => DigItem(direction match {
        case "R" => Right
        case "L" => Left
        case "U" => Up
        case "D" => Down
      }, s.toInt, color)
    }.toArray

  }

  override def subDir: String = "eighteen"
}

case class DigItem(dir: Direction, steps: Int, color: String)

object Point {
  def move(pos: (Int, Int), dir: Direction, steps: Int = 1): (Int, Int) = {
    val (x, y) = pos
    Iterator(
      ((x, y - steps), Left), ((x, y + steps), Right), ((x - steps, y), Up), ((x + steps, y), Down)
    ).find(v => v._2 == dir).get._1
  }
}