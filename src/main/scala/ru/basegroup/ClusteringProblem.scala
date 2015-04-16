package ru.basegroup

/**
 * Описание задачи кластеризации
 * @param transactionSource источник транзакций
 * @param objectsInTransactionCount количество объектов в транзакции
 * @tparam T тип объектов транзакций
 */
case class ClusteringProblem[T](transactionSource: TransactionsSource[T], objectsInTransactionCount:Option[Int]=None)
