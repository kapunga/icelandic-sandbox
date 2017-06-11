package org.kapunga.icelandic

import java.security.MessageDigest



/**
  * @author Paul J Thordarson kapunga@gmail.com
  */
package object util {
  def md5(s: String): String = BigInt(1, MessageDigest.getInstance("MD5").digest(s.getBytes)).toString(36)
}
