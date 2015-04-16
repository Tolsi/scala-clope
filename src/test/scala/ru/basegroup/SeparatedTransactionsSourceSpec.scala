package ru.basegroup

import org.scalatest.{Matchers, FlatSpec}
import scala.io.Source

class SeparatedTransactionsSourceSpec extends FlatSpec with Matchers {
  it should "возвращать размер 1" in {
    val source = new SeparatedTransactionsSource(Source.fromString("1,2,3,4,5,6"))
    source.size should be(1)
  }
  it should "возвращать размер 2" in {
    val source = new SeparatedTransactionsSource(Source.fromString("1,2,3\n4,5,6"))
    source.size should be(2)
  }

  it should "давать пройти дважды" in {
    val source = new SeparatedTransactionsSource(Source.fromString("1,2,3\n4,5,6"))
    source.size should be(2)
    source.size should be(2)
  }
}
