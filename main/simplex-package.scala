package object simplex {
  def x(n: Int) = Term(1.0, Var(n))

  def maximize(expr: LinearExpr) = new Maximizer(expr)
  def minimize(expr: LinearExpr) = new Maximizer(-expr)

  implicit class DoubleForLinearExpr(val self: Double) extends AnyVal {
    def *(e: LinearExpr) = e * self
  }

  implicit class IntForLinearExpr(val self: Int) extends AnyVal {
    def *(e: LinearExpr) = e * self.toDouble
  }
}
