object ProblemSheet1 {
    def main(args: Array[String]) : Unit = {
        ...
    }

    def Q1square(x: Int) : Int = x * x

    def Q1remainderDiv3(x: Int): Int = {
        val r = x - (x/3) * 3
        if (r >= 0) {
            r
        } else {
            r+3
        }
    }
    // x/3 is integer division, so 10/3 = 3. 10-3 = 1 - correct
    

    def Q1getLargestPerfectSquare(x: Int): Int = {
        require(x >= 0)
        var i = 0
        while ((i+1)*(i+1) < x) i+=1
        i
    }

    def Q2milkBillTotal(milks: Array[Int]) : Int = {
        var i = milks.length; var total = 0
        // Invariant: (0 <= i <= milks.length) ^ (total = sum(milks[i..n)))
        
        // Variant: i
        while (i > 0) {
            i = i - 1
            total += milks(i)
        }
        total
    }

    def Q3maxMilks(milks: Array[Int]) : Int = {
        var maxMilk = 0; var i = 0; val n = milks.length
        require(n > 0)  // Doesn't make sense to get max of nothing. 

        // Variant: n - i
        // Invariant: (0 <= i <= n) ^ (maxMilk = maxElement(milks[0..i)))
        while (i < n) {
            if (milks(i) > maxMilk) maxMilk = milks(i)
            i += 1
        }
        maxMilk
    }

    /*
    Q4:
    - Since the size of the array is set, you could use a for loop in findSum. 
    - You create an array and loop over it unnecessarially. You could just pass
      args to findSum and convert to integers there as you loop through it. This 
      saves space through not making a new array and saves time by not looping over
      arrays of length n unnecessarially. 
    - You get the size of equally-sized arrays twice - this could be fixed as above.
    */

    def Q5recursiveFib(x: Int) : Int = {
        require(x >= 0)
        if (x == 0) return 0
        if (x == 1) return 1
        Q5recursiveFib(x-1) + Q5recursiveFib(x-2)
    }

    def Q5recursiveFibTree(x: Int) : (Array[String], Int) = {
        if (x <= 1) {
            return (Array(s"fib($x)", s"= $x"), 0)
        }
        var (bigStr, bigVal) = Q5recursiveFibTree(x-1)
        var (smallStr, smallVal) = Q5recursiveFibTree(x-2)

        val newVal = bigVal + smallVal
        (
            Array(
                Array(s"fib($x)"), 
                bigStr.map(line => "| " + line), 
                smallStr.map(line => "| " + line), 
                Array(s"= $newVal")
            ).flatten, newVal
        )
    }

    def Q5outputTree(x: Int) : Unit = {
        require(x>=0)
        for (line <- Q5recursiveFibTree(x)._1) println(line)
    }

    def Q6nonRecursiveFib(x: Int) : Int = {
        require(x>=0)
        if (x <= 1) return x
        
        var fibNums = new Array[Int](x+1)
        fibNums(0) = 0; fibNums(1) = 1
        
        var i = 2
        // Invariant: (2<=i<=x+1) ^ (fibNums[0..i) are the 0th - i-1th fib. nums)
        while (i <= x) {
            fibNums(i) = fibNums(i-1) + fibNums(i-2)
            i += 1
        }
        fibNums(x)
    }

    def Q7moduloAndDiv(x: Int, y: Int) : (Int, Int) = {
        require(x>=0 && y>0)
        var r = x; var q = 0
        // Invariants: (r + q * y == x) ^ (0 <= r < x) ^ (0 <= q <= div x y)
        while (x - q * y >= y) {
            r -= y
            q += 1
        }
        (r, q)
    }

    def Q8GCD(a: Int, b: Int) : Int = {
        require(a >= b && b > 0)

        val (r, _) = Q7moduloAndDiv(a, b)
        if (r == 0) return b
        Q8GCD(b, r)
    } 

    def Q8GCDWithBezout(a: Int, b: Int) : (Int, Int, Int) = {
        require(a >= b && b > 0)

        val (r, d) = Q7moduloAndDiv(a, b)
        if (r == 0) return (b, 0, 1)
        val (answer, x, y) = Q8GCDWithBezout(b, r)

        /* 
        b * d + r = a
        answer = xb + yr
        need to write r in terms of a, b
        r = a - b * y. Therefore answer = xb + y(a-b*d) = ya + (x-y*d)b
        */

        (answer, y, x-y*d)
    } 

    def Q9hitCounter(arr: Array[Int]) : Int = {
        val n = arr.length
        if (n==0) return 0

        var hits = 1
        var bigHit = arr(0)
        var i = 1

        // Invariant: (bigHit == max(arr[0:i)) ^ (1 <= i <= n) ^ (hits = count(arr(j) > each arr[0..j) for j = 0 to i-1))
        while (i < n) {
            if (arr(i) > bigHit) {
                hits += 1
                bigHit = arr(i)
            }
            i += 1
        }
        hits

    }
}