package ru.basegroup

import scala.collection.parallel.ForkJoinTaskSupport

/**
 * Решатель задачи кластеризации
 * @tparam T тип объектов транзакций
 */
trait ClusteringProblemResolver[T] {

  /**
   * Решает задачу кластеризации
   * @param problem задача
   * @return решение задачи
   */
  def solve(problem: ClusteringProblem[T]): ClusteringResult[T]
}
