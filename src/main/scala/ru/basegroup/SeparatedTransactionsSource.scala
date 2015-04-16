package ru.basegroup

import scala.io.Source
import scala.Predef._

class SeparatedTransactionsSource(source: => Source, transactionsLimit: Option[Int] = None, separator: String = ",") extends TransactionsSource[String] {

  private var lines: Iterator[String] = _

  def reset() = lines = source.getLines()

  def iterator: Iterator[Transaction[String]] = {
    reset()
    val result = lines.map(_.split(separator).filter(_ != "?"))
    transactionsLimit.map(result.take(_)) getOrElse result
  }
}
