package com.github
package matticala.scala.util

import java.io._
import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, SecureRandom}

import scala.util.Try

final class UUID (val msb: Long, val lsb: Long) extends Serializable with Equals with Comparable[UUID] {

  import UUID.hex

  val version: Int = ((msb >> 12) & 0x0f).toInt

  override val toString: String = s"${hex(msb >> 32, 8)}-${hex(msb >> 16, 4)}-${hex(msb, 4)}-${hex(lsb >> 48, 4)}-${hex(lsb, 12)}"

  override def compareTo(o: UUID): Int = {
    val msb = this.msb compareTo o.msb
    if (msb == 0) this.lsb compareTo o.lsb else msb
  }

  def asJava: java.util.UUID = UUID.toJava(this)

  def toBytes: Bytes = this.msb.toBytes ++ this.lsb.toBytes

  def canEqual(other: Any): Boolean = other.isInstanceOf[UUID] || other.isInstanceOf[java.util.UUID]

  override def equals(that: Any): Boolean = that match {
    case that: UUID ⇒ (this eq that) || (this.msb == that.msb && this.lsb == that.lsb)
    case that: java.util.UUID ⇒ this.msb == that.getMostSignificantBits && this.lsb == that.getLeastSignificantBits
    case _ ⇒ false
  }

  override def hashCode(): Int = {
    val xor = msb ^ lsb
    (xor >> 32).toInt ^ xor.toInt
  }

  def copy(msb: Long = this.msb, lsb: Long = this.lsb) = new UUID(msb, lsb)

  def tupled: (Long, Long) = (msb, lsb)
}

object UUID {
  import java.lang.Long.BYTES

  private lazy val generator: SecureRandom = new SecureRandom()

  final val namespaceAndMD5: String ⇒ String ⇒ UUID = namespaceUUID(MessageDigest.getInstance("MD5").digest)(3)

  final val namespaceAndSHA1: String ⇒ String ⇒ UUID = namespaceUUID(MessageDigest.getInstance("SHA").digest)(5)

  final val namespaceAndSHA224: String ⇒ String ⇒ UUID = namespaceUUID(MessageDigest.getInstance("SHA-224").digest)(5)

  final val namespaceAndSHA256: String ⇒ String ⇒ UUID = namespaceUUID(MessageDigest.getInstance("SHA-256").digest)(5)

  final val namespaceAndSHA384: String ⇒ String ⇒ UUID = namespaceUUID(MessageDigest.getInstance("SHA-384").digest)(5)

  final val namespaceAndSHA512: String ⇒ String ⇒ UUID = namespaceUUID(MessageDigest.getInstance("SHA-512").digest)(5)

  final val version4: UUID = random

  final def version3(namespace: String, name: String): UUID = namespaceAndMD5(namespace)(name)

  final def version5(namespace: String, name: String): UUID = namespaceAndSHA1(namespace)(name)

  def unapply(arg: UUID): Option[(String)] = Some(arg.toString)

  def unapply(arg: String): Option[UUID] = UUID.fromString(arg).toOption

  def random: UUID = {
    val array: Bytes = Array.ofDim(2*BYTES)
    generator.nextBytes(array)
    UUID.fromBytes(setUUIDBytes(array, 4))
  }

  def fromString(string: String): Try[UUID] = Try {
    val parts: Array[String] = string.split("-")
    require(parts.length == 5, s"Invalid UUID string: $string")
    val numericParts: Array[Long] = parts.map(java.lang.Long.valueOf(_, 16).toLong)
    new UUID(
      (((numericParts(0) << 16) | numericParts(1)) << 16) | numericParts(2),
      (numericParts(3) << 48) | numericParts(4)
    )
  }

  implicit def toJava(uuid: UUID): java.util.UUID = new java.util.UUID(uuid.msb, uuid.lsb)

  implicit def fromJava(uuid: java.util.UUID): UUID = new UUID(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)

  private def fromBytes(array: Bytes): UUID = {
    require(array.length > 15, "Data must be 16+ bytes long")
    val (msb, lsb) = array.take(2*BYTES).splitAt(BYTES)
    new UUID(bytes2long(msb), bytes2long(lsb))
  }

  private def namespaceUUID(digest: Bytes ⇒ Bytes)(version: Byte)(namespace: String)(name: String): UUID = {
    val array = digest(namespace.getBytes(StandardCharsets.UTF_8) ++ name.getBytes(StandardCharsets.UTF_8))
    UUID.fromBytes(setUUIDBytes(array, version))
  }

  private def setUUIDBytes(source: Bytes, version: Int): Bytes = {
    val bytes = new Bytes(source.length)
    Array.copy(source, 0, bytes, 0, source.length)
    bytes(6) = ((bytes(6) & 0x0f) | (version << 4)).toByte
    bytes(8) = ((bytes(8) & 0x3f) | 0x80).toByte
    bytes
  }

  private def bytes2long(bytes: Bytes): Long = bytes.take(BYTES).foldLeft(0L)((acc, byte) ⇒ (acc << BYTES) | (byte & 0xff))

  private def hex(value: Long, digits: Int): String = {
    val hi: Long = 1L << (digits * 4)
    (hi | (value & (hi - 1))).toHexString.substring(1)
  }
}
