package simplex.doubleerror

case class Epsilon(value: Double)

object Epsilon {
  implicit val defaultEpsilon = Epsilon(1.0e-8)
}
