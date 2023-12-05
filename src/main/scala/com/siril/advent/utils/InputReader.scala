package com.siril.advent.utils

import com.siril.advent.tasks.Task

import scala.io.Source

object InputReader {

  def readLines[T](fileName: String, task: Task[T, _]): T =
    task.parseInput(readFile(s"tasks/${task.subDir}/$fileName").toList)

  private def readFile(path: String) =
    Source.fromResource(path).getLines
}

