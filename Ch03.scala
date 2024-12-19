import scala.math.sqrt

object Ch03:
  def average(x: Double, y: Double): Double =
    (x + y) / 2.0

  def pythagorean(a: Double, b: Double): Double =
    sqrt(a*a + b*b)

  def milesPerGallon(initialOdometerReading: Double,
                     finalOdometerReading: Double,
                     gallonsConsumed: Double): Double =
    (finalOdometerReading - initialOdometerReading) / gallonsConsumed

  @annotation.tailrec
  def isLongerThan[A](l1: List[A], l2: List[A]): Boolean =
    if l1.isEmpty then false
    else if l2.isEmpty then true
    else isLongerThan(l1.tail, l2.tail)
