import simplex._

object Main extends App {
  /*
  // http://zeus.mech.kyushu-u.ac.jp/~tsuji/java_edu/Simplex_st.html
  maximize (2*x(0) + 3*x(1)) subjectTo {
      x(0) + 2*x(1) <= 14
      x(0) +   x(1) <= 8
    3*x(0) +   x(1) <= 18
  }

  // www.fujilab.dnj.ynu.ac.jp/lecture/system2.pdf
  maximize (400*x(0) + 300*x(1)) subjectTo {
    60*x(0) + 40*x(1) <= 3800
    20*x(0) + 30*x(1) <= 2100
    20*x(0) + 10*x(1) <= 1200
  }

  // http://dic.nicovideo.jp/a/%E7%B7%9A%E5%BD%A2%E8%A8%88%E7%94%BB%E5%95%8F%E9%A1%8C
  maximize (x(0) + x(1) + x(2)) subjectTo {
    5*x(0) +   x(1) + 2*x(2) <= 20
    2*x(0) + 2*x(1) + 6*x(2) <= 30
    2*x(0) + 6*x(1) + 4*x(2) <= 40
  }
   */

  // http://www.bunkyo.ac.jp/~nemoto/lecture/or/99/simplex2.pdf
  maximize (-6*x(0) + 6*x(1)) subjectTo {
    2*x(0) + 3*x(1) <= 6
    -5*x(0) + 9*x(1) == 15
    -6*x(0) + 3*x(1) >= 3
  }
}
