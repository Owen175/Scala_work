object Factorial {
    def factorial (n: Int) : BigInt = {
        require (n >= 0)
        if (n==0) 1
        else factorial (n-1) * n
    }

    def main (args: Array[String]) : Unit = {
        print("Please input a number: ")
        val n = scala.io.StdIn.readInt()
        val f = factorial(n)
        println("The factorial of " + n + " is " + f)
    }
}