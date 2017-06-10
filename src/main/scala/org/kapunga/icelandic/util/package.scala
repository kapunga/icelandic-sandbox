package org.kapunga.icelandic

import java.security.MessageDigest



/**
  * Created by kapunga on 6/10/17.
  */
package object util {
  def md5(s: String): String =
    BigInt(1, MessageDigest.getInstance("MD5").digest(s.getBytes)).toString(36)
}
