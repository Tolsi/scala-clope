package ru.basegroup

import org.scalatest._
import scala.io.Source

class ClusterSpec extends FlatSpec with Matchers {

  implicit val p = Precision(0.01)

  it should "высчитывать profit - 1" in {
    val cluster1 = new Cluster[String]()
    cluster1.addTransaction(Transaction("a", "b", "c"))
    cluster1.addTransaction(Transaction("a", "b", "c", "d"))
    cluster1.addTransaction(Transaction("b", "c", "d", "e"))
    cluster1.addTransaction(Transaction("c", "d", "e"))

    val profit = CLOPEResolver.profit(Array(cluster1), 3.107)
    assert(profit != 0)
    assert(!profit.isNaN)
    assert(CLOPEResolver.profit(Array(cluster1), 1) ~== 2.8)
  }

  it should "высчитывать profit - 2" in {
    val cluster1 = new Cluster[String]()
    cluster1.addTransaction(Transaction("a", "b", "c"))
    cluster1.addTransaction(Transaction("a", "b", "c", "d"))

    val cluster2 = new Cluster[String]()
    cluster2.addTransaction(Transaction("b", "c", "d", "e"))
    cluster2.addTransaction(Transaction("c", "d", "e"))
    val profit = CLOPEResolver.profit(Array(cluster1, cluster2), 3.107)
    profit should not be (0)
    assert(!profit.isNaN)
    assert(CLOPEResolver.profit(Array(cluster1), 1) ~== 1.75)
  }

  it should "высчитывать profit - 3" in {
    val cluster1 = new Cluster[String]()
    cluster1.addTransaction(Transaction("a", "b", "c"))
    cluster1.addTransaction(Transaction("a", "b", "c", "d"))
    cluster1.addTransaction(Transaction("b", "c", "d", "e"))
    cluster1.addTransaction(Transaction("c", "d", "e"))

    val cluster2 = new Cluster[String]()
    cluster2.addTransaction(Transaction("a", "b", "c"))
    cluster2.addTransaction(Transaction("a", "b", "c", "d"))

    val cluster3 = new Cluster[String]()
    cluster3.addTransaction(Transaction("b", "c", "d", "e"))
    cluster3.addTransaction(Transaction("c", "d", "e"))



    CLOPEResolver.profit(Array(cluster1), 3.107) should be < CLOPEResolver.profit(Array(cluster2, cluster3), 3.107)
  }

  it should "высчитывать profit пустого кластера" in {
    val cluster1 = new Cluster[String]()

    assert(!CLOPEResolver.profit(Array(cluster1), 3.107).isNaN)
    assert(CLOPEResolver.profit(Array(cluster1), 3.107) == 0)
  }

  it should "высчитывать параметры кластера - 1" in {
    val cluster1 = new Cluster[String]()
    val transactionSource = new SeparatedTransactionsSource(Source.fromString("a,b\na,b,c\na,c,d"))
    transactionSource.foreach(transaction => cluster1.addTransaction(transaction))
    assert(cluster1.elements.count("a") === 3)
    assert(cluster1.elements.count("b") === 2)
    assert(cluster1.elements.count("c") === 2)
    assert(cluster1.elements.count("d") === 1)
    assert(cluster1.s === 8)
    assert(cluster1.h === 2)
    assert(cluster1.w === 4)
  }

  it should "высчитывать параметры кластера - 2" in {
    val cluster1 = new Cluster[String]()
    val transactionSource = new SeparatedTransactionsSource(Source.fromString("d,e\nd,e,f"))
    transactionSource.foreach(transaction => cluster1.addTransaction(transaction))

    assert(cluster1.h ~= 1.67)
    assert(cluster1.w == 3)
  }

  //  it should "решать задачу грибов за несколько итераций" in {
  //    val mushroomsTransactionsSource = new SeparatedTransactionsSource(Source.fromURL("http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"))
  //    val solver = new CLOPEResolver(transactionSource = mushroomsTransactionsSource, reductionCoefficient = 2.6d, clustersCount = 27)
  //    val clusters = solver.solve()
  //    clusters.zipWithIndex.foreach(zipped => println(zipped._2 + 1, zipped._1.size))
  //  }
}
