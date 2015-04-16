package ru.basegroup

import org.scalatest._
import scala.io.Source

class CLOPEResolverSpec extends FlatSpec with Matchers {

  it should "решать задачу грибов на 1 итерации верно" in {
    val mushroomsTransactionsSource = new SeparatedTransactionsSource(Source.fromURL(getClass.getResource("/agaricus-lepiota.data")))
    val mushroomsProblem = ClusteringProblem(transactionSource = mushroomsTransactionsSource, Some(2))
    val solver = new CLOPEResolver[String](reductionCoefficient = 1.8d,
      clustersCount = 27)
    val results = solver.solveWithLimit(mushroomsProblem, 1)
    import results.clusters
    //clusters.zipWithIndex.foreach(zipped => println(zipped._2 + 1, zipped._1.size))
    assert(clusters.size > 1)
    //CLOPEResolver.dertyClusters(clusters) should be < 2
    //    clusters.map(_.size).deep should be(
    //      Array(256, 512, 768, 96, 96, 192, 1296, 432, 149, 192, 1146, 1, 288, 192, 223, 48, 72, 80, 8, 8, 1497, 192, 288,
    //        32, 36, 8, 16).deep
    //    )
  }

  it should "не терять транзакции" in {
      val mushroomsTransactionsSource = new SeparatedTransactionsSource(Source.fromURL(getClass.getResource("/agaricus-lepiota.data")))
      val mushroomsProblem = ClusteringProblem(transactionSource = mushroomsTransactionsSource, Some(2))
      val solver = new CLOPEResolver[String](reductionCoefficient = 1.8d,
        clustersCount = 27)
      val results = solver.solveWithLimit(mushroomsProblem, 1)
      import results.clusters
      clusters.map(_.size).sum should be(8124)
  }

  it should "решить Надину задачу с r=3" in {
    val transactionSource = new SeparatedTransactionsSource(Source.fromString("r,b,c,d\na,c,d,f\nr,b,c,e\ne,c,d"))
    val problem=ClusteringProblem(transactionSource)
    val solver = new CLOPEResolver[String](reductionCoefficient = 3, clustersCount = 2)
    val results = solver.solve(problem)
    import results.clusters
    assert(results.transactionsIdToClusters.get(0).get === clusters(0))
    assert(results.transactionsIdToClusters.get(1).get === clusters(1))
    assert(results.transactionsIdToClusters.get(2).get === clusters(0))
    assert(results.transactionsIdToClusters.get(3).get === clusters(0))
  }

  it should "решать задачу грибов верно - 1" in {
    val mushroomsTransactionsSource = new SeparatedTransactionsSource(Source.fromURL(getClass.getResource("/agaricus-lepiota.data")))
    val mushroomsProblem = ClusteringProblem(transactionSource = mushroomsTransactionsSource, Some(1))
    val solver = new CLOPEResolver[String](reductionCoefficient = 2.8d,
      clustersCount = 27)
    val result = solver.solve(mushroomsProblem)
    import result.clusters

    assert(clusters.size > 1)
    clusters.map(_.w).max should be(1)
  }

  it should "решать задачу грибов верно - 2" in {
    val mushroomsTransactionsSource = new SeparatedTransactionsSource(Source.fromURL(getClass.getResource("/agaricus-lepiota.data")))
    val mushroomsProblem = ClusteringProblem(transactionSource = mushroomsTransactionsSource, Some(2))
    val solver = new CLOPEResolver[String](reductionCoefficient = 2.8d,
      clustersCount = 27)
    val result = solver.solve(mushroomsProblem)
    import result.clusters

    assert(clusters.size > 1)
    clusters.map(_.w).max should be(2)
  }

  it should "высчитывать грязные блоки верно" in {
    val dertyCluster1 = new Cluster[String]
    dertyCluster1.addTransaction(Transaction("a", "b"))
    dertyCluster1.addTransaction(Transaction("b"))

    val dertyCluster2 = new Cluster[String]
    dertyCluster2.addTransaction(Transaction("d", "c"))
    dertyCluster2.addTransaction(Transaction("y"))

    val cleanCluster = new Cluster[String]
    cleanCluster.addTransaction(Transaction("a", "a"))
    cleanCluster.addTransaction(Transaction("a"))

    CLOPEResolver.dertyClusters(Array(cleanCluster)) should be(0)
    CLOPEResolver.dertyClusters(Array(dertyCluster1, cleanCluster)) should be(1)
    CLOPEResolver.dertyClusters(Array(dertyCluster1, dertyCluster2, cleanCluster)) should be(2)
  }
}
