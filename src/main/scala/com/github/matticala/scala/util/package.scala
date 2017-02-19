package com.github
package matticala
package scala

import java.lang.Long.BYTES

package object util {

  type Bytes = Array[Byte]

  implicit class ExtLong(val underlying: Long) extends AnyVal {
    def toBytes: Bytes = Array.tabulate(BYTES)(i ⇒ ((underlying >> (BYTES - 1 - i) * BYTES) & 0xff).toByte)
  }

}
