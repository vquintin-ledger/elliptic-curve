import algebra.Group
import algebra.ring.Field

sealed abstract class ECPoint[+A]

import spire.implicits._

object ECPoint {

  final case class ECPointOnCurve[+A](x: A, y: A) extends ECPoint[A]

  case object ECInfinity extends ECPoint[Nothing]

  def groupECPoint[A](a: A, b: A)(implicit F: Field[A]): Group[ECPoint[A]] = {
    new Group[ECPoint[A]] {
      override def inverse(a: ECPoint[A]): ECPoint[A] =
        a match {
          case ECInfinity => ECInfinity
          case ECPointOnCurve(x, y) => ECPointOnCurve(x, -y)
        }

      override def empty: ECPoint[A] = ECInfinity

      override def combine(p: ECPoint[A], q: ECPoint[A]): ECPoint[A] =
        (p, q) match {
          case (ECInfinity, q) => q
          case (p, ECInfinity) => p
          case (p: ECPointOnCurve[A], q: ECPointOnCurve[A]) if p.x != q.x =>
            val slope = (p.y - q.y) / (p.x - q.x)
            fromSlope(slope, p, q)
          case (p: ECPointOnCurve[A], q: ECPointOnCurve[A]) if p == inverse(q) =>
            ECInfinity
          case (p: ECPointOnCurve[A], q: ECPointOnCurve[A]) /* p == q */ =>
            val slope = (3 * p.x * p.x + a) / (2 * p.y)
            fromSlope(slope, p, q)
        }

      def fromSlope(slope: A, p: ECPointOnCurve[A], q: ECPointOnCurve[A]): ECPointOnCurve[A] = {
        val xR = slope * slope - p.x - q.x
        val yR =  p.y + slope * (xR - p.x)
        ECPointOnCurve(xR, -yR)
      }
    }
  }
}
