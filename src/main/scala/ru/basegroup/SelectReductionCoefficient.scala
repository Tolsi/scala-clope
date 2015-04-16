package ru.basegroup

import scala.io.Source

object ReductionCoefficientFinder {
  def main(args: Array[String]) {
    val finder = new ReductionCoefficientFinder(-10, 10)
    println(finder.find())
  }
}

class ReductionCoefficientFinder(from: Double, to: Double, step: Double = 0.1) {

  def find(): Double = {
    (from to to by step).par.maxBy(i => {
      val mushroomsTransactionsSource = new SeparatedTransactionsSource(Source.fromURL(getClass.getResource("/agaricus-lepiota.data")))
      val mushroomsProblem = ClusteringProblem(transactionSource = mushroomsTransactionsSource, Some(2))
      val solver = new CLOPEResolver[String](reductionCoefficient = i,
        clustersCount = 27)
      val result = solver.solve(mushroomsProblem)
      val clusters = result.clusters
      clusters.size * (1/clusters.map(_.w).sum)
    })
  }
}
