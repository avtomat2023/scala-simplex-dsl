package simplex

import scala.annotation._
import scala.collection.mutable
import implicits._

case class Var(n: Int) {
  require(n >= 0)
}

sealed trait Equality
case object Le extends Equality
case object Ge extends Equality
case object Eq extends Equality

sealed trait LinearExpr {
  def +(other: LinearExpr): LinearExpr = new +(this, other)
  def *(other: Double): LinearExpr
  def unary_- : LinearExpr
  private[simplex] def makeCoeffs: Map[Int,Double]
  private[simplex] def makeConstraint(const: Double, e: Equality): Constraint =
    Constraint(makeCoeffs, const, e)
  def <=(const: Double) {
    require(Maximizer.constraintsRef.nonEmpty)
    Maximizer.constraintsRef.head += makeConstraint(const, Le)
  }
  def >=(const: Double) {
    require(Maximizer.constraintsRef.nonEmpty)
    Maximizer.constraintsRef.head += makeConstraint(const, Ge)
  }
  def ==(const: Double) {
    require(Maximizer.constraintsRef.nonEmpty)
    Maximizer.constraintsRef.head += makeConstraint(const, Eq)
  }
}

case class Term(coeff: Double, variable: Var) extends LinearExpr {
  def unary_- : LinearExpr = Term(-coeff, variable)
  def *(other: Double) = Term(coeff * other, variable)
  def makeCoeffs: Map[Int,Double] = Map(variable.n -> coeff)
}

case class +(lhs: LinearExpr, rhs: LinearExpr) extends LinearExpr {
  def unary_- : LinearExpr = new +(-lhs, -rhs)
  def *(other: Double) = new +(lhs * other, rhs * other)
  def makeCoeffs: Map[Int,Double] = (lhs.makeCoeffs mix rhs.makeCoeffs)(_ + _)
}

/** Constraint `expr (<= or >= or ==) const` */
case class Constraint(
  coeffs: Map[Int,Double], const: Double, equality: Equality
)

// http://zeus.mech.kyushu-u.ac.jp/~tsuji/java_edu/Simplex_st.html
class Tableau(expr: LinearExpr, constraints: Seq[Constraint]) {
  val Epsilon = 1.0e-8
  val Fine = 1.0e-6

  val nObjectiveVars = constraints.flatMap(_.coeffs.keySet).max + 1
  val nSlackVars = constraints.length
  val nArtificialVars = constraints.filter(_.equality != Le).length
  val nAllVars = nObjectiveVars + nSlackVars + nArtificialVars
  val contents =
    Array.fill(constraints.length + 1)(
      Array.fill(nAllVars + 1)(0.0)
    )

  // set the first row
  (-expr).makeCoeffs.foreach { case (n, coeff) =>
    contents(0)(n) = coeff
  }

  // set rows since the second one
  private[this] val s0 = nObjectiveVars
  private[this] val a0 = nObjectiveVars + nSlackVars
  private[this] var iArtificial = 0
  constraints.zipWithIndex.foreach{ case (constraint, i) =>
    val row = i + 1
    constraint.coeffs.foreach{ case (n, coeff) =>
      contents(row)(n) = coeff
    }
    constraint.equality match {
      case Le =>
        contents(row)(s0 + i) = 1.0
      case Ge =>
        contents(row)(s0 + i) = -1.0
        contents(row)(a0 + iArtificial) = 1.0
        contents(0)(a0 + iArtificial) = Fine
        iArtificial += 1
      case Eq =>
        contents(row)(a0 + iArtificial) = 1.0
        contents(0)(a0 + iArtificial) = Fine
        iArtificial += 1
    }
    contents(row)(nAllVars) = constraint.const
  }

  def objectiveRow: Array[Double] = contents(0)
  def objectiveValue: Double = objectiveRow(nAllVars)

  def selectCol: Int = objectiveRow.zipWithIndex.minBy(_._1)._2
  def isSolved(col: Int): Boolean = objectiveRow(col) >= -Epsilon
  def selectRow(col: Int): Int =
    contents.zipWithIndex
      .drop(1)
      .filter{ case (row, _) => row(col) > Epsilon }
      .minBy { case (row, _) => row(nAllVars) / row(col)}
      ._2

  def sweep(iRow: Int, iCol: Int) {
    val pivotRow = contents(iRow)
    val pivot = pivotRow(iCol)
    (0 to nAllVars).foreach { i =>
      pivotRow(i) /= pivot
    }

    contents.filter(row => !(row eq pivotRow)).foreach { row =>
      val modifier = row(iCol)
      (0 to nAllVars).foreach { i =>
        row(i) -= pivotRow(i) * modifier
      }
    }
  }

  @tailrec final def simplex() {
    print()
    println()
    val col = selectCol
    if (!isSolved(col)) {
      val row = selectRow(col)
      sweep(row, col)
      simplex()
    }
  }

  /** For debug. */
  def print() {
    val stringTable = contents.map { row =>
      row.map { value =>
        f"$value%1.2f"
      }
    }
    val maxLengths = (0 to nAllVars).map { iCol =>
      val col = (0 until contents.length).map { iRow =>
        stringTable(iRow)(iCol)
      }
      col.map(_.length).max
    }
    stringTable.foreach { row =>
      (row zip maxLengths).foreach { case (s, len) =>
        Predef.print(" " * (len - s.length) + s + " ")
      }
      println()
    }
  }

  def result: Map[Int,Double] = {
    val assocs = (0 until nAllVars).flatMap(n => solutionOf(n).map(v => n->v))
    Map(assocs: _*)
  }

  def solutionOf(n: Int): Option[Double] = {
    require(0 <= n && n < nAllVars)
    val col = contents.map(row => row(n))
    val nonZeros = col.zipWithIndex.filter{ case (x,i) =>
      x < -Epsilon || Epsilon < x
    }
    if (nonZeros.length == 1) {
      val (value, index) = nonZeros.head
      if (-Epsilon < value-1.0 && value-1.0 < Epsilon && index != 0)
        Some(contents(index)(nAllVars))
      else None
    } else None
  }
}

class Maximizer(expr: LinearExpr) {
  /** Solves the problem and prints the solution. */
  def subjectTo(constraintsRegisterer: => Unit) {
    val constraints = registerConstraints(constraintsRegisterer)
    val tableau = new Tableau(expr, constraints)
    tableau.simplex()
    printSolution(tableau)
  }

  private[simplex]
  def registerConstraints(registerer: => Unit): Seq[Constraint] = {
    require(Maximizer.constraintsRef.isEmpty)
    Maximizer.constraintsRef = Some(mutable.Buffer.empty[Constraint])
    registerer
    val constraints = Maximizer.constraintsRef.head
    Maximizer.constraintsRef = None
    constraints
  } ensuring(ret => Maximizer.constraintsRef.isEmpty)

  @tailrec private[simplex] final
  def simplexMethod(tableau: Tableau) {
    tableau.print()
    println()
    val col = tableau.selectCol
    if (!tableau.isSolved(col)) {
      val row = tableau.selectRow(col)
      tableau.sweep(row, col)
      simplexMethod(tableau)
    }
  }

  private def printSolution(tableau: Tableau) {
    println("optimized value of objective function: " + tableau.objectiveValue)
    println("value of variables:")
    val result = tableau.result

    def slack(n: Int) = if (n < tableau.nObjectiveVars) "" else "(slack)"
    result.toSeq.sortBy(_._1).foreach { case (n, value) =>
      println("x" + n + slack(n) + " = " + value)
    }
  }
}

object Maximizer {
  private[simplex] var constraintsRef: Option[mutable.Buffer[Constraint]] = None
}
