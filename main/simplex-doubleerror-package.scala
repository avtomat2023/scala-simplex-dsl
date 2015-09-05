package simplex

package object doubleerror {
  /** Allows comparison of doubles taking errors in calculation into accont. */
  implicit class DoubleWithError(val self: Double) extends AnyVal {
    /** `a >~ b` means "probably `a > b`" */
    def >~(other: Double)(implicit eps: Epsilon): Boolean =
      self - other > -eps.value
    /** `a <~ b` means "probably `a < b`" */
    def <~(other: Double)(implicit eps: Epsilon): Boolean =
      other - self > -eps.value
    /** `a =~ b` means "probably `a == b`" */
    def =~(other: Double)(implicit eps: Epsilon): Boolean =
      this >~ other && this <~ other

    /** `a |>| b` means "It's impossible that a <= b" */
    def |>|(other: Double)(implicit eps: Epsilon): Boolean =
      self - other > eps.value
    /** `a |<| b` means "It's impossible that a >= b" */
    def |<|(other: Double)(implicit eps: Epsilon): Boolean =
      other - self > eps.value
    /** `a |!=| b` means "It's impossible that a == b" */
    def |!=|(other: Double)(implicit eps: Epsilon): Boolean = !(this =~ other)
  }
}
