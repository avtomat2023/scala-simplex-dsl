package simplex

import org.scalatest._
import org.scalatest.Matchers._

class TestSimplex extends FunSuite with Matchers {
  test("expr creation") {
    val e = -(x(0) + 3*x(1)) * 2
    assert(e == new +(Term(-2.0, Var(0)), Term(-6.0, Var(1))))
  }

  test("make constraint from expr") {
    val e = -(x(0) + 3*x(1)) * 2 + x(0)
    val c = e.makeConstraint(5.0, Le)
    assert(c == Constraint(Map(0 -> -1.0, 1 -> -6.0), 5.0, Le))
  }

  test("constraint registration") {
    val m = maximize(2*x(0) + 3*x(1))
    val cs = m.registerConstraints {
        x(0) + 2*x(1) <= 14
        x(0) +   x(1) <= 8
      3*x(0) +   x(1) <= 18
    }
    assert(cs == Seq(
      Constraint(Map(0 -> 1.0, 1 -> 2.0), 14.0, Le),
      Constraint(Map(0 -> 1.0, 1 -> 1.0), 8.0, Le),
      Constraint(Map(0 -> 3.0, 1 -> 1.0), 18.0, Le)
    ))
  }

  // http://zeus.mech.kyushu-u.ac.jp/~tsuji/java_edu/Simplex_st.html
  val expr = 2*x(0) + 3*x(1)
  val constraints = Seq(
    Constraint(Map(0 -> 1.0, 1 -> 2.0), 14.0, Le),
    Constraint(Map(0 -> 1.0, 1 -> 1.0), 8.0, Le),
    Constraint(Map(0 -> 3.0, 1 -> 1.0), 18.0, Le)
  )
  def makeTableau = new Tableau(expr, constraints)

  test("tableau initialization") {
    val t = Array(
      Array(-2.0, -3.0, 0.0, 0.0, 0.0, 0.0),
      Array( 1.0,  2.0, 1.0, 0.0, 0.0, 14.0),
      Array( 1.0,  1.0, 0.0, 1.0, 0.0, 8.0),
      Array( 3.0,  1.0, 0.0, 0.0, 1.0, 18.0)
    )
    assertResult(t){ makeTableau.contents }
  }

  test("Tableau#objectiveRow") {
    val row = Array(-2.0, -3.0, 0.0, 0.0, 0.0, 0.0)
    assertResult(row){ makeTableau.objectiveRow }
  }

  test("Tableau#selectCol") {
    assertResult(1){ makeTableau.selectCol }
  }

  test("Tableau#selectRow") {
    assertResult(1){ makeTableau.selectRow(1) }
  }

  test("Tableau#sweep") {
    val tableau = makeTableau
    tableau.sweep(1,1)
    val t = Array(
      Array(-0.5, 0.0,  1.5, 0.0, 0.0, 21.0),
      Array( 0.5, 1.0,  0.5, 0.0, 0.0, 7.0),
      Array( 0.5, 0.0, -0.5, 1.0, 0.0, 1.0),
      Array( 2.5, 0.0, -0.5, 0.0, 1.0, 11.0)
    )
    assertResult(t){ tableau.contents }
  }

  test("solve") {
    val m = maximize(expr)
    val tableau = makeTableau
    val map = Map(1 -> 6.0, 0 -> 2.0, 4 -> 6.0)
    m.simplexMethod(tableau)
    assertResult(map){ tableau.result }

    val t = Array(
      Array(0.0, 0.0,  1.0,  1.0, 0.0, 22.0),
      Array(0.0, 1.0,  1.0, -1.0, 0.0, 6.0),
      Array(1.0, 0.0, -1.0,  2.0, 0.0, 2.0),
      Array(0.0, 0.0,  2.0, -5.0, 1.0, 6.0)
    )
    assertResult(t){ tableau.contents }
  }
}
