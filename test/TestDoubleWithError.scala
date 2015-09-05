package simplex.doubleerror

import org.scalatest.FunSuite

class TestDoubleWithError extends FunSuite {
  import Epsilon.defaultEpsilon
  val eps = defaultEpsilon.value / 2.0
  val delta = defaultEpsilon.value * 2.0

  test("operator '<~'") {
    val x = 33.4
    assert(x <~ x+eps)
    assert(x <~ x-eps)
    assert(x <~ x+delta)
    assert(!(x <~ x-delta))
  }

  test("operator '=~'") {
    val x = 42.0
    assert(x =~ x+eps)
    assert(x =~ x-eps)
    assert(!(x =~ x+delta))
    assert(!(x =~ x-delta))
  }

  test("operator '|<|'") {
    val x = 33.4
    assert(!(x |<| x+eps))
    assert(!(x |<| x-eps))
    assert(x |<| x+delta)
    assert(!(x |<| x-delta))
  }

  test("operator '|!=|'") {
    val x = 42.0
    assert(!(x |!=| x+eps))
    assert(!(x |!=| x-eps))
    assert(x |!=| x+delta)
    assert(x |!=| x-delta)
  }
}
