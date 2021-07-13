import org.scalatest.funsuite.AnyFunSuite

import spire.implicits._

class ECPointTest extends AnyFunSuite {

  // Example from the bitcoin book
  test("Works with secp256k1") {
    val a = BigInt(0)
    val b = BigInt(7)
    val k = BigInt("1E99423A4ED27608A15A2616A2B0E9E52CED330AC530EDCC32C8FFC6A526AEDD", 16)
    val G = ECPoint.ECPointOnCurve[BigInt](
      BigInt("79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798", 16),
      BigInt("483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8", 16)
    )
    val n = BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F", 16)
    val F = GaloisField.fieldP(n)
    val group = ECPoint.groupECPoint[BigInt](a, b)(F)
    val expected = ECPoint.ECPointOnCurve[BigInt](
      BigInt("F028892BAD7ED57D2FB57BF33081D5CFCF6F9ED3D3D7F159C2E2FFF579DC341A", 16),
      BigInt("07CF33DA18BD734C600B96A72BBC4749D5141C90EC8AC328AE52DDFE2E505BDB", 16)
    )

    val actual = Helper.times[ECPoint[BigInt], BigInt](G, k)(group, implicitly)

    assert(actual == expected)
  }
}
