package ru.basegroup

import scala.collection.parallel.immutable.ParSeq
import java.util.concurrent.atomic.AtomicInteger
import java.util.Date
import scala.collection.mutable

object CLOPEResolver {
  def profit[T](clusters: Seq[Cluster[T]], reductionCoefficient: Double): Double = {
    val numerator = clusters.par.foldLeft(0d) {
      (sum, cluster) =>
        sum + cluster.g(reductionCoefficient) * cluster.size
    }
    val denominator = clusters.foldLeft(0d) {
      (sum, cluster) => sum + cluster.size
    }
    val result = numerator / denominator
    if (result.isNaN) 0 else result
  }

  def dertyClusters[T](clusters: Seq[Cluster[T]]): Int = {
    clusters.par.foldLeft(0) {
      (sum, cluster) =>
        sum + implicitly[Int](cluster.w > 1)
    }
  }

  def bestClustering[T](reductionCoefficient: Double, results: ClusteringResult[T]*) {
    results.maxBy(result => profit(result.clusters, reductionCoefficient))
  }
}

/**
 * Решатель задач кластеризации методом CLOPE
 * @param reductionCoefficient коэффициент отскока
 * @param clustersCount максимальное число кластеров
 * @tparam T тип объектов кластера
 */
class CLOPEResolver[T](reductionCoefficient: Double, clustersCount: Int) extends ClusteringProblemResolver[T] {

  private var transactionsToClustersMap: mutable.SynchronizedMap[Int, Cluster[T]] = _
  private var clusters: ParSeq[Cluster[T]] = _
  private var problem: ClusteringProblem[T] = _

  private var moved = false
  private val iterationCount = new AtomicInteger(0)

  private[basegroup] def iteration(): Seq[Cluster[T]] = {
    // при распараллеливании меняется порядок транзакций и этого влияет на результат
    problem.transactionSource./*par.*/zipWithIndex.foreach(zipped => {
      val (transaction, transactionId) = zipped

      val currentCluster = transactionsToClustersMap.get(transactionId)
      val bestCluster = findBestClusterToAdd(transaction, currentCluster)

      if (currentCluster.isEmpty || currentCluster.get != bestCluster) {
        if (!moved)
          moved = true

        moveTransaction(transaction, transactionId, bestCluster, currentCluster)
      }
    })
    clusters.toBuffer
  }

  private[basegroup] def findBestClusterToAdd(transaction: Transaction[T], currentCluster: Option[Cluster[T]] = None): Cluster[T] = {
    clusters.par.maxBy(cluster =>
      cluster.deltaAdd(transaction, reductionCoefficient, problem.objectsInTransactionCount) +
        currentCluster.map(_.deltaRemove(transaction, reductionCoefficient)).getOrElse(0d))
  }

  private def moveTransaction(transaction: Transaction[T], transactionIndex: Int, toCluster: Cluster[T], fromCluster: Option[Cluster[T]] = None) {
    transactionsToClustersMap += transactionIndex -> toCluster

    fromCluster.map(_.removeTransaction(transaction, problem.objectsInTransactionCount))
    toCluster.addTransaction(transaction, problem.objectsInTransactionCount)
  }

  private[basegroup] def solveWithLimit(problem: ClusteringProblem[T], iterationsLimit: Int = 10): ClusteringResult[T] = {
    this.synchronized {
      this.problem = problem
      clusters = ParSeq.fill(clustersCount)(new Cluster[T]())
      transactionsToClustersMap = new mutable.HashMap[Int, Cluster[T]] with mutable.SynchronizedMap[Int, Cluster[T]]

      println(new Date)
      do {
        prepare()
        iteration()
      } while (moved && iterationCount.get() < iterationsLimit)

      val result = ClusteringResult(clusters = clusters.filter(_.size > 0).toBuffer, transactionsIdToClusters =
        transactionsToClustersMap.toMap,
        iterations = iterationCount.get())

      clean()

      println(new Date)
      result
    }
  }

  private def clean() {
    clusters = null
    transactionsToClustersMap = null
    problem = null
  }

  private def prepare() {
    moved = false
    problem.transactionSource.reset()
    iterationCount.incrementAndGet()
  }

  /**
   * Решает задачу кластеризации
   * @param problem задача
   * @return решение задачи
   */
  def solve(problem: ClusteringProblem[T]): ClusteringResult[T] = solveWithLimit(problem)
}
