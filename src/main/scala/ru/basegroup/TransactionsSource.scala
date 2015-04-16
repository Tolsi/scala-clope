package ru.basegroup

trait TransactionsSource[T] extends Iterable[Transaction[T]] {

  def reset()

}
