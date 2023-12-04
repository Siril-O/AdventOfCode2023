package com.siril.advent.tasks

trait Task[T, R] extends (T => R)
