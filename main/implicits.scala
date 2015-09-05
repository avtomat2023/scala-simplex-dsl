import scala.collection.mutable

package object implicits {
  implicit class MyRichMap[K,V](val self: Map[K,V]) extends AnyVal {
    def mix(other: Map[K,V])(f: (V,V) => V): Map[K,V] = {
      val result = mutable.Map(self.toSeq: _*)
      other.toSeq.foreach { case (k, v) =>
        if (result.contains(k))
          result(k) = f(result(k), v)
        else
          result(k) = v
      }
      Map(result.toSeq: _*)
    }
  }

  implicit class MyRichSeq[A](val self: Seq[A]) extends AnyVal {
    def zipWithIndexFrom(start: Int): Seq[(A,Int)] =
      self.zipWithIndex.map{ case (a, i) => (a, start + i) }
  }

  def optIf[A](cond: Boolean)(body: => Option[A]): Option[A] =
    if (cond) body else None

  implicit class MyRichBoolean(val self: Boolean) extends AnyVal {
    def thenSome[A](a: => A) = if (self) Some(a) else None
  }
}
