object Dynamic extends App {
  def longestCommonSubstring(a: String, b: String): String = {
    val dp = Array.ofDim[Int](a.length, b.length)
    for {
      i <- a.indices
      j <- b.indices
    } {
      if (a(i) == b(j))
        dp(i)(j) = 1 + (if (i > 0 && j > 0) dp(i - 1)(j - 1) else 0)
      else
        dp(i)(j) = 0
    }

    dp.foreach(arr => println(arr.mkString))

    // Find max
    var maxv = 0
    var maxi = -1
    var maxj = -1

    for {
      i <- a.indices
      j <- b.indices
    } {
      if (dp(i)(j) > maxv) {
        maxv = dp(i)(j)
        maxi = i
        maxj = j
      }
    }
    (maxi - maxv + 1 to maxi).map(i => a(i)).mkString
  }

  // https://leetcode.com/problems/longest-common-subsequence
  def longestCommonSubsequence(a: String, b: String): Int = {
    // DP[i][j] represents the longest common subsequence of text1[0 ... i] & text2[0 ... j].
    val dp = Array.ofDim[Int](a.length, b.length)
    for {
      i <- a.indices
      j <- b.indices
    } {
      val prev = math.max(
        if (i > 0) dp(i - 1)(j) else 0,
        if (j > 0) dp(i)(j - 1) else 0
      )

      if (a(i) == b(j))
        dp(i)(j) = 1 + (if (i > 0 && j > 0) dp(i - 1)(j - 1) else 0)
      else
        dp(i)(j) = prev
    }

    //dp.foreach(arr => println(arr.mkString))

    // Find max
    var maxv = 0

    for {
      i <- a.indices
      j <- b.indices
    } {
      if (dp(i)(j) > maxv) {
        maxv = dp(i)(j)
      }
    }
    maxv
  }

  println(longestCommonSubsequence("abcde", "ace"))          // 3
  println(longestCommonSubsequence("abc", "abc"))            // 3
  println(longestCommonSubsequence("abc", "def"))            // 0
  println(longestCommonSubsequence("bsbininm", "jmjkbkjkv")) // 1
}
