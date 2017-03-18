package com.github
package matticala
package scala

import java.lang.Long.BYTES
import java.util.Random

package object util {

  type Bytes = Array[Byte]

  implicit class ExtLong(val underlying: Long) extends AnyVal {
    def toBytes: Bytes = Array.tabulate(BYTES)(i ⇒ ((underlying >> (BYTES - 1 - i) * BYTES) & 0xff).toByte)
  }

  implicit class ExtRandom(val underlying: Random) extends AnyVal {

    def nextBytes(size: Int): Bytes = {
      val bytes: Bytes = Array.ofDim(size)
      underlying.nextBytes(bytes)
      bytes
    }

  }

}
