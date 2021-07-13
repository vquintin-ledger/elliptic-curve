import spire.algebra.{Eq, Field}

import spire.implicits._

object GaloisField {

  // Give an implementation of field for finite fields
  def fieldP(prime: BigInt): Field[BigInt] =
    new Field[BigInt] {
      override def gcd(a: BigInt, b: BigInt)(implicit ev: Eq[BigInt]): BigInt = a.gcd(b)

      override def lcm(a: BigInt, b: BigInt)(implicit ev: Eq[BigInt]): BigInt = div(times(a, b), gcd(a, b))

      override def times(x: BigInt, y: BigInt): BigInt = (x * y).mod(prime)

      override def negate(x: BigInt): BigInt = prime - x

      override val zero: BigInt = BigInt(0)

      override def div(x: BigInt, y: BigInt): BigInt = times(x, reciprocal(y))

      override def reciprocal(x: BigInt): BigInt = {
        implicit val G = multiplicative
        // x^-1 == x^(prime - 2)
        Helper.times[BigInt, BigInt](x, prime - 2)
      }
      override val one: BigInt = BigInt(1)

      override def plus(x: BigInt, y: BigInt): BigInt = (x + y).mod(prime)
    }
}
