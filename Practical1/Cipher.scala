object Cipher {
/** Bit-wise exclusive-or of two characters */
  def xor(a: Char, b: Char) : Char = (a.toInt ^ b.toInt).toChar

  /** Print ciphertext in octal */
  def showCipher(cipher: Array[Char]) =
    for(c <- cipher){ print(c/64); print(c%64/8); print(c%8); print(" ") }

  /** Read file into array */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray

  /** Read from stdin in a similar manner */
  def readStdin() = scala.io.Source.stdin.toArray

  /* ----- Functions below here need to be implemented ----- */

  /** Encrypt plain using key; can also be used for decryption */
  def encrypt(key: Array[Char], plain: Array[Char]) : Array[Char] = {
    val key_len = key.length; val plain_len = plain.length
    var output_array = new Array[Char](plain_len)
    for (i <- 0 until plain.length) {
        output_array(i) = xor(plain(i), key(i%key_len))
    }
    output_array
  }

  /** Try to decrypt ciphertext, using crib as a crib */
  def tryCrib(crib: Array[Char], ciphertext: Array[Char]) : Unit = {
    val crib_len = crib.length; val cipher_len = ciphertext.length
    var j = -1
    var shift = 0
    var kcs = new Array[Char](crib_len)
    for (start <- 0 until (cipher_len - crib_len)) {
      kcs = encrypt(ciphertext.slice(start, start + crib_len), crib)
      j = repeatedChars(kcs, crib_len)
      if (j != -1) {
        var shifted_kcs = new Array[Char](j)
        shift = start % j
        for (i<-0 until j) {
          shifted_kcs((i - shift + j +2 )%j) = kcs(i)
        }
        
        println(new String(shifted_kcs))
        println(new String(encrypt(shifted_kcs, ciphertext)))
      }
    }
  }
  
  def repeatedChars(key_chars: Array[Char], len: Int) : Int = {
    for (j <- 1 to len - 2) {
      if (key_chars.slice(0, len - j).sameElements((key_chars.slice(j, len)))) {
        return j
      }
    }
    -1
  }

  /** The first optional statistical test, to guess the length of the key */
  def crackKeyLen(ciphertext: Array[Char]) : Unit = {
    var count = 0; val ciphertext_len = ciphertext.length
    for (shift <- 1 to 30) {
      count = 0
      for (i <- 0 to ciphertext_len - 31) {
        if (ciphertext(i) == ciphertext(shift + i)) {
          count += 1
        }
      }
      println(s"$shift: $count")
    }
  }

  /** The second optional statistical test, to guess characters of the key. */
  def crackKey(klen: Int, ciphertext: Array[Char]) : Unit = {
    val ciphertext_len = ciphertext.length
    var char = ' '
    for (s <- klen until ciphertext_len by klen) {
      for (i <- 0 until ciphertext_len - s) {
        if (ciphertext(i) == ciphertext(i+s)) {
          char = xor(' ', ciphertext(i))
          if (32 <= char.toInt && 127 >= char.toInt) {
            val keyIdx = i % klen
            println(s"$keyIdx $char")
          }
        }
      }
    }
  }

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) : Unit = {
    // string to print if error occurs
    val errString = 
      "Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
      "     | scala Cipher -crib crib [file]\n"+
      "     | scala Cipher -crackKeyLen [file]\n"+
      "     | scala Cipher -crackKey len [file]"

    // Get the plaintext, either from the file whose name appears in position
    // pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else readStdin()

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val command = args(0)
    if(command=="-encrypt" || command=="-decrypt"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      print(new String (encrypt(key,plain)))
    }
    else if(command=="-crib"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      tryCrib(key, plain)
    }
    else if(command=="-crackKeyLen"){
      checkNumArgs(1); val plain = getPlain(1)
      crackKeyLen(plain)
    }      
    else if(command=="-crackKey"){
      checkNumArgs(2); val klen = args(1).toInt; val plain = getPlain(2)
      crackKey(klen, plain)
    }
    else println(errString)
  }
}

// PEMBERLEY
// HOGWARTS
