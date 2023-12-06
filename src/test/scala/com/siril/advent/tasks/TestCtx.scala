package com.siril.advent.tasks

import com.siril.advent.utils.InputReader
import org.specs2.matcher.Scope

trait TestCtx[I,O] extends Scope {
  def instance: Task[I, O]

  val testInput: I = InputReader.readLines("test.txt", instance)
  val taskInput: I = InputReader.readLines("inputA.txt", instance)
}

