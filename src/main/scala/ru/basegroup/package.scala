package ru


package object basegroup {
  type Transaction[T] = Array[T]

  object Transaction {}

  implicit def TransactionToArray(x: Transaction.type) = Array

  implicit def bool2int(b: Boolean) = if (b) 1 else 0

  case class Precision(val p: Double)
  class withAlmostEquals(d: Double) {
    def ~=(d2: Double)(implicit p: Precision) = (d - d2).abs <= p.p

    def ~==(d2: Double)(implicit p: Precision): Option[String] =
      if (~=(d2))
        None
      else {
        Some(s"$d is not equals $d2")
      }
  }
  implicit def add_~=(d: Double) = new withAlmostEquals(d)

}
