import java.security.MessageDigest

object MD5 {

  /** Hex string representation of a MD5 hashed string.
    * @param s
    * @return
    */
  def md5(s: String): String = {
    val md = MessageDigest.getInstance("MD5")
    md.digest(s.getBytes()).map(0xff & _).map {
        "%02x".format(_)
      }.foldLeft("") {
        _ + _
      }
  }

}
