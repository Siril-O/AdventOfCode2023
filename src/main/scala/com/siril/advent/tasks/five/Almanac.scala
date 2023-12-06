package com.siril.advent.tasks.five

import scala.collection.mutable.ArrayBuffer

case class Almanac(
                    seeds: Array[Long],
                    mappings: Array[Mapping]
                  )

case class Mapping(srcAttr: Attribute, destAttr: Attribute, ranges: Array[ValuesRange]) {
  def findCorrespondingDest(src: Long): Long =
    ranges.find(_.isInRange(src)).map(_.diff + src).getOrElse(src)

  def findCorrespondingDestRange(wholeRange: (Long, Long)): Array[(Long, Long)] = {
    val intersected = new ArrayBuffer[(Long, Long)]()
    var leftovers = Array(wholeRange)

    ranges.filter(_.hasIntersection(wholeRange)).foreach { range =>
      leftovers = leftovers.flatMap { leftover =>
        if (range.hasIntersection(leftover)) {
          intersected += range.toDest(range.intersect(wholeRange))
          range.diff(leftover)
        } else Array(leftover)
      }
    }
    leftovers ++ intersected
  }
}

case class ValuesRange(destinationStart: Long, sourceStart: Long, length: Long) {
  val diff: Long = destinationStart - sourceStart
  private val sourceEnd = sourceStart + length

  def isInRange(src: Long): Boolean =
    (src >= sourceStart) && (src <= sourceStart + length)

  def hasIntersection(range: (Long, Long)): Boolean =
    range._1 <= sourceEnd && range._2 >= sourceStart

  def toDest(range: (Long, Long)): (Long, Long) =
    (range._1 + diff, range._2 + diff)

  def intersect(range: (Long, Long)): (Long, Long) =
    Math.max(sourceStart, range._1) -> Math.min(sourceStart + length, range._2)

  def diff(range: (Long, Long)): Array[(Long, Long)] = {
    val left = if (range._1 < sourceStart) Array((range._1, sourceStart)) else Array.empty[(Long, Long)]
    val right = if (range._2 > sourceEnd) Array((sourceEnd, range._2)) else Array.empty[(Long, Long)]
    left ++ right
  }

}

object ValuesRange {

  def apply(raw: String): ValuesRange = raw.trim.split(" ").toList match {
    case List(d, s, l) => ValuesRange(d.toLong, s.toLong, l.toLong)
  }
}
