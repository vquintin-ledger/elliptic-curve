import spire.algebra.{EuclideanRing, Monoid}

object Helper {

  // multiply or pow, depending on G
  def times[A, B](p: A, n: B)(implicit G: Monoid[A], ER: EuclideanRing[B]): A = {
    val two = ER.additive.combineN(ER.one, 2)
    def go(n: B): A = {
      val (q, r) = ER.equotmod(n, two)
      val halfSum = if(q == ER.zero) G.empty else go(q)
      val d = G.combine(halfSum, halfSum)
      if (r == ER.zero)
        d
      else
        G.combine(d, p)
    }
    go(n)
  }
}
