package ru.basegroup

import scala.collection._
import com.google.common.collect.{HashMultiset, Multiset}
import scala.collection.JavaConverters._
import Math.pow

case class Cluster[T]() {

  var size = 0
  val elements: Multiset[T] = HashMultiset.create()

  def addTransaction(transaction: Transaction[T], objectsInTransactionCount: Option[Int] = None) = {
    val objectsCount = checkObjectsInTransaction(transaction, objectsInTransactionCount)
    transaction.take(objectsCount).foreach(elements.add(_))
    size += 1
  }

  def removeTransaction(transaction: Transaction[T], objectsInTransactionCount: Option[Int] = None) = {
    val objectsCount = checkObjectsInTransaction(transaction, objectsInTransactionCount)
    transaction.take(objectsCount).foreach(elements.remove(_))
    size -= 1
  }

  def s: Int = elements.size()

  def d: Set[T] = elements.elementSet().asScala

  def w: Int = d.size

  def h: Double = s.toDouble / w

  def g(reductionCoefficient: Double): Double = {
    if (s == 0 && w == 0)
      0
    else
      s.toDouble / Math.pow(w.toDouble, reductionCoefficient)
  }

  private def checkObjectsInTransaction[T](transaction: Transaction[T], objectsInTransactionCount: Option[Int]): Int = {
    if (objectsInTransactionCount.getOrElse(0) > transaction.size) transaction.size else objectsInTransactionCount.getOrElse(0)
  }

  def deltaAdd(transaction: Transaction[T], reductionCoefficient: Double, objectsInTransactionCount: Option[Int] = None): Double = {
    val objectsCount = checkObjectsInTransaction(transaction, objectsInTransactionCount)
    val news = s.toDouble + objectsCount
    val newUniqueValuesCount = transaction.take(objectsCount).filter(elements.count(_) == 0).size
    val neww = w.toDouble + newUniqueValuesCount
    val newProfit = news * (size + 1) / pow(neww, reductionCoefficient)
    var currProfit = s * size / pow(w, reductionCoefficient)
    if (currProfit.isNaN) currProfit = 0
    newProfit - currProfit
  }

  def deltaRemove(transaction: Transaction[T], reductionCoefficient: Double, objectsInTransactionCount: Option[Int] = None): Double = {
    val objectsCount = checkObjectsInTransaction(transaction, objectsInTransactionCount)
    val news = s.toDouble - objectsCount
    val neww = w.toDouble - transaction.take(objectsCount).filter(elements.count(_) == 1).size

    val newProfit = news * (size - 1) / pow(neww, reductionCoefficient)
    var currProfit = s * size / pow(w, reductionCoefficient)
    if (currProfit.isNaN) currProfit = 0

    newProfit - currProfit
  }
}