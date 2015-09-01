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
    val c = e.makeConstraint(5.0)
    assert(c == Constraint(Map(0 -> -1.0, 1 -> -6.0), 5.0))
  }

  test("constraint registration") {
    val m = maximize(2*x(0) + 3*x(1))
    val cs = m.registerConstraints {
        x(0) + 2*x(1) <= 14
        x(0) +   x(1) <= 8
      3*x(0) +   x(1) <= 18
    }
    assert(cs == Seq(
      Constraint(Map(0 -> 1.0, 1 -> 2.0), 14.0),
      Constraint(Map(0 -> 1.0, 1 -> 1.0), 8.0),
      Constraint(Map(0 -> 3.0, 1 -> 1.0), 18.0)
    ))
  }

  // http://zeus.mech.kyushu-u.ac.jp/~tsuji/java_edu/Simplex_st.html
  val expr = 2*x(0) + 3*x(1)
  val constraints = Seq(
    Constraint(Map(0 -> 1.0, 1 -> 2.0), 14.0),
    Constraint(Map(0 -> 1.0, 1 -> 1.0), 8.0),
    Constraint(Map(0 -> 3.0, 1 -> 1.0), 18.0)
  )
  def makeTabulaue = new Tabulaue(expr, constraints)

  test("tabulaue initialization") {
    val t = Array(
      Array(-2.0, -3.0, 0.0, 0.0, 0.0, 0.0),
      Array( 1.0,  2.0, 1.0, 0.0, 0.0, 14.0),
      Array( 1.0,  1.0, 0.0, 1.0, 0.0, 8.0),
      Array( 3.0,  1.0, 0.0, 0.0, 1.0, 18.0)
    )
    assertResult(t){ makeTabulaue.contents }
  }

  test("Tabulaue#objectiveRow") {
    val row = Array(-2.0, -3.0, 0.0, 0.0, 0.0, 0.0)
    assertResult(row){ makeTabulaue.objectiveRow }
  }

  test("Tabulaue#selectCol") {
    assertResult(1){ makeTabulaue.selectCol }
  }

  test("Tabulaue#selectRow") {
    assertResult(1){ makeTabulaue.selectRow(1) }
  }

  test("Tabulaue#sweep") {
    val tabulaue = makeTabulaue
    tabulaue.sweep(1,1)
    val t = Array(
      Array(-0.5, 0.0,  1.5, 0.0, 0.0, 21.0),
      Array( 0.5, 1.0,  0.5, 0.0, 0.0, 7.0),
      Array( 0.5, 0.0, -0.5, 1.0, 0.0, 1.0),
      Array( 2.5, 0.0, -0.5, 0.0, 1.0, 11.0)
    )
    assertResult(t){ tabulaue.contents }
  }

  test("solve") {
    val m = maximize(expr)
    val tabulaue = makeTabulaue
    val map = Map(1 -> 6.0, 0 -> 2.0, 4 -> 6.0)
    m.simplexMethod(tabulaue)
    assertResult(map){ tabulaue.result }

    val t = Array(
      Array(0.0, 0.0,  1.0,  1.0, 0.0, 22.0),
      Array(0.0, 1.0,  1.0, -1.0, 0.0, 6.0),
      Array(1.0, 0.0, -1.0,  2.0, 0.0, 2.0),
      Array(0.0, 0.0,  2.0, -5.0, 1.0, 6.0)
    )
    assertResult(t){ tabulaue.contents }
  }
}
