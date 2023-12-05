package com.siril.advent.tasks

trait Task[T, R] extends (T => R) {
  def parseInput(raw: List[String]): T

  def subDir: String
}
