package ru.basegroup

/**
 * Решение задачи кластеризации
 * @param clusters кластеры
 * @param transactionsIdToClusters карта, содержащая пренадлежность номера транзакции и кластера
 * @param iterations количество итераций
 * @tparam T тип объектов транзакций
 */
case class ClusteringResult[T](clusters: Seq[Cluster[T]], transactionsIdToClusters:Map[Int, Cluster[T]], iterations:Int)
