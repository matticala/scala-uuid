package com.github.matticala.scala
package util

import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, SecureRandom}

import scala.util.Try

sealed class UUID (val msb: Long, val lsb: Long) extends Serializable with Equals with Comparable[UUID] {

  import UUID.Private.hex

  val version: Int = ((msb >> 12) & 0x0f).toInt

  override val toString: String = s"${hex(msb >> 32, 8)}-${hex(msb >> 16, 4)}-${hex(msb, 4)}-${hex(lsb >> 48, 4)}-${hex(lsb, 12)}"

  override def compareTo(o: UUID): Int = {
    val msb = this.msb compareTo o.msb
    if (msb == 0) this.lsb compareTo o.lsb else msb
  }

  override def equals(that: Any): Boolean = that match {
    case that: UUID ⇒ (this eq that) || (this.msb == that.msb && this.lsb == that.lsb)
    case that: java.util.UUID ⇒ this.msb == that.getMostSignificantBits && this.lsb == that.getLeastSignificantBits
    case _ ⇒ false
  }

  override def hashCode(): Int = {
    val xor = msb ^ lsb
    (xor >> 32).toInt ^ xor.toInt
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[UUID] || other.isInstanceOf[java.util.UUID]

  def copy(msb: Long = this.msb, lsb: Long = this.lsb) = new UUID(msb, lsb)

  def toBytes: Bytes = this.msb.toBytes ++ this.lsb.toBytes

  def tupled: (Long, Long) = (msb, lsb)

  implicit def asJava: java.util.UUID = new java.util.UUID(this.msb, this.lsb)
}

object UUID {

  case object Nil extends UUID(0, 0)

  import Private._

  val namespaceAndMD5: String ⇒ String ⇒ UUID = namespaceAndNameUUID(MessageDigest.getInstance("MD5").digest)(3)

  val namespaceAndSHA1: String ⇒ String ⇒ UUID = namespaceAndNameUUID(MessageDigest.getInstance("SHA").digest)(5)

  val namespaceAndSHA256: String ⇒ String ⇒ UUID = namespaceAndNameUUID(MessageDigest.getInstance("SHA-256").digest)(8)

  val namespaceAndSHA512: String ⇒ String ⇒ UUID = namespaceAndNameUUID(MessageDigest.getInstance("SHA-512").digest)(9)

  val randomOnNamespace: String ⇒ UUID = namespaceUUID(MessageDigest.getInstance("SHA-256").digest)(8)(_)()

  val version4: UUID = random

  def version3(namespace: String, name: String): UUID = namespaceAndMD5(namespace)(name)

  def version5(namespace: String, name: String): UUID = namespaceAndSHA1(namespace)(name)

  def random: UUID = fromBytes(setUUIDBytes(getRandomBytes, 4))

  def fromString(string: String): Try[UUID] = Try {
    val parts: Array[String] = string.split("-")
    require(parts.length == 5, s"Invalid UUID string: $string")
    val numericParts: Array[Long] = parts.map(java.lang.Long.valueOf(_, 16).toLong)
    new UUID(
      (((numericParts(0) << 16) | numericParts(1)) << 16) | numericParts(2),
      (numericParts(3) << 48) | numericParts(4)
    )
  }

  def unapply(arg: UUID): Option[(String)] = Some(arg.toString)

  def unapply(arg: String): Option[UUID] = UUID.fromString(arg).toOption

  implicit def fromJava(uuid: java.util.UUID): UUID = new UUID(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)

  private[UUID] object Private {

    import java.lang.Long.BYTES

    private lazy val generator: SecureRandom = new SecureRandom()

    def getRandomBytes: Bytes = generator.nextBytes(2*BYTES)

    def fromBytes(array: Bytes): UUID = {
      val (msb, lsb) = array.take(2*BYTES).splitAt(BYTES)
      new UUID(bytes2long(msb), bytes2long(lsb))
    }

    def namespaceAndNameUUID(digest: Bytes ⇒ Bytes)(version: Byte)(namespace: String)(name: String): UUID =
      namespaceUUID(digest)(version)(namespace)(name.getBytes(StandardCharsets.UTF_8))

    def namespaceUUID(digest: Bytes ⇒ Bytes)(version: Byte)(namespace: String)(addition: Bytes = getRandomBytes): UUID = fromBytes {
      val array = digest(namespace.getBytes(StandardCharsets.UTF_8) ++ addition)
      setUUIDBytes(array, version)
    }

    def setUUIDBytes(source: Bytes, version: Int): Bytes = {
      val bytes = new Bytes(source.length)
      Array.copy(source, 0, bytes, 0, source.length)
      bytes(6) = ((bytes(6) & 0x0f) | (version << 4)).toByte
      bytes(8) = ((bytes(8) & 0x3f) | 0x80).toByte
      bytes
    }

    def bytes2long(bytes: Bytes): Long = bytes.take(BYTES).foldLeft(0L)((acc, byte) ⇒ (acc << BYTES) | (byte & 0xff))

    def hex(value: Long, digits: Int): String = {
      val hi: Long = 1L << (digits * 4)
      (hi | (value & (hi - 1))).toHexString.substring(1)
    }
  }
}
