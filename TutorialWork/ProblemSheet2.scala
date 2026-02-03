/*
1a
it swaps the integer values stored in two memory addresses labelled x and y

1b
it swaps the data stored at index i in the array with the data stored at index j

2
It will print 7
If you swap the order, it will print 11. 
*/

// 3
import org.scalatest.Funsuite.AnyFunSuite

class Question4 extends AnyFunSuite{
    test("orders single letters correctly") {assert(Array("a", "c", "b", "d", "f", "e").sorted == Array("a", "b", "c", "d", "e", "f"))}
    test("orders words correctly") {assert(Array("scala", "cipher", "quicksort", "Scala", "Quicksort", "siphon").sorted == Array("Quicksort", "Scala", "cipher", "quicksort", "scala", "siphon"))}
    test("order by length") {assert(Array("666666", "333", "1", "55555", "4444", "22").sortBy(_.length) == Array("1", "22", "333", "4444", "55555", "666666"))}
}

/*
4:
If timeStep is even slightly less than the true value of timeEnd/numSteps due to a rounding error, 
time will be slightly less than timeEnd when timeStep is added numSteps times, resulting in another addition. 

if the rounding error is r, time will be r * numSteps off, assuming the loop exits at the right time. 

Rather than re-adding the timeStep with the rounding error of r, you could recompute time each iteration with 
a fresh division - introducing an integer count variable which is incremented each cycle, you could set time to 
(count.toDouble/numSteps) * timeEnd. This would lead to no rounding error at the end, since (count.toDouble/numSteps) * timeEnd = timeEnd 
when count = numSteps, assuming toDouble does not have rounding errors for integers. 
However, this is not particularly fast, since the division operation on floating point numbers is not fast. 
*/

// 5
/** Does patt appear as a substring of line? */

def search(patt: Array[Char], line: Array[Char]) : Boolean = {
    val K = patt.size; val N = line.size
    // Invariant: I: found = (line[i..i+K) = patt[0..K) for
    // some i in [0..j)) and 0 <= j <= N-K
    var j = 0; var found = false
    while(j <= N-K && !found){
        // set found if line[j..j+K) = patt[0..K)
        // Invariant: line[j..j+k) = patt[0..k)
        var k = 0
        while(k<K && line(j+k)==patt(k)) k = k+1
        found = (k==K)
        j = j+1
    }
    // I && (j=N-K+1 || found)
    // found = ( line[i..i+K) = patt[0..K) for some i in [0..N-K+1) )
    found
}



class Question5 extends AnyFunSuite{
    test("Line 6") {assert(search("SmallString", "BigString") == true)}
    // Would come out as true no matter what
    test("Line 7(a)") {assert(search("World!", "Hello World!") == true)}
    // Doesn't check the last possible position the substring could be in
    test("Line 7(b)") {noException should be thrownBy {search("notHere", "Hello World!")}}
    // Would be an exception, since line(N) would be called, resulting in an idx out of bounds error
    test("Line 10") {assert(search("Borld!", "World!") == false)}
    // The first character is not checked - these would come up as equal
    test("Line 11") {noException should be thrownBy {search("World!", "Hello World!")}}
    // Could also check that the test output would be true for a substring in the middle of the string as K==k would be false
    // Or that it is true when the last character is wrong. 
    // Line 12 would not be a bug just poorly written code. k should never exceed K so equality is 
    // the same as >=
}

// Q6

object Q6to10 {
    def main(args: Array[String]) : Unit = {
        println(q10PolynomialEval(Array(1.0, 2.0, 3.0), 10))

    }
    def q6smallestRecurrence(charArr: Array[Char]) : Int = {
        var n = 1
        var N = charArr.length
        var c = 0
        var valid = true
        // Invariant: 1 <= n <= N, no recurrence with num =1 to n-1 is valid
        while (n < N) {
            c = 0
            valid = true
            // Invariant: N-n >= c >= 0 ^ valid = &&(charArr(i), charArr(i+n)) for all i < c
            while (c < N-n) {
                if (charArr(c) != charArr(n+c)) valid = false
                c += 1
            }
            if (valid) return n
            n += 1
        }
        N
    }

    def q7any(p: Int=>Boolean, N: Int) : Boolean = {
        var i = 0
        // Invariant: 0 <= i <= N ^ none of [0..i) satisfy p(x)
        while (i < N) {
            if (p(i)) return true
            i += 1
        }
        false

        // Reasoning for correctness: I test each i in [0..n) and return true if p(i) is true. 
        // The invariant in combination with the loop condition show that i == N if the while loop ends
        // naturally. Therefore, none of [0..N) satisfy p(i), so false should be returned.
    }

    def q8DirectReciprocals(p: Int, q: Int) : Array[Int] = {
        // m>=q/p and is the smallest integer such that that is true.
        // m = ceil(q/p) 
        // Integer division in scala is truncation - not ideal
        // q/p is correct for q is a multiple of p, otherwise 1 too few. 
        // ((q-1) / p) + 1 is correct 
        // val m = (q-1) / p + 1   Only correct if q > 0, p > 0 which are both true throughout the loop
        
        // Setting up variables with correct vals. 
        
        var d = new Array[Int](100)  // Assuming will have enough space as the PS said
        var m = (q-1) / p + 1
        d(0) = m

        var oldP = p; var oldQ = q
        var newp = oldP * m - oldQ; var newq = m * oldQ

        var count = 1
        // Invariant: q > 0, p > 0, initialFraction = p/q + sum(1/d(i)) over i in [0, count) 
        while (newp != 0) {
            m = (newq-1) / newp + 1
            oldP = newp; oldQ = newq
            newp = oldP * m - oldQ; newq = m * oldQ
            // Proof of reduction
            /*  
            p * m = q-1 + p    due to integer division
            newp = p * m - q = p-1.

            P is decreasing by one each time, meaning that it will eventually hit 0 since it is a finite integer. 
            */
            d(count) = m
            count += 1
        }
        d
    // proof d is strictly increasing:
    /*
    d are the values of m: the smallest number whose reciprocal was smaller than or equal to p/q. 
    p/q is decreasing as you iterate as you repeatedly subtract a positive number from it. 
    therefore, m must be increasing (or potentially staying equal throughout some iterations).
    Now just need to show that m is not staying equal: p/q - 1/m > 1/m. 
    p/q > 2/m
    This is evidently true by contradiction
    as p/q < 1, m > 1, meaning that there is an integer smaller than m and greater or equal to m/2.
    Therefore, 1/m can be replaced by 1/a when a is that integer, unless m = 2, but in that case, 
    p/q > 2/m since 2/m = 1.
    However, this is a contradiction, since a would be a smaller integer than m such that 1/a < p/q. 
    Therefore, p/q > 2/m, so p/q - 1/m > 1/m. 
    Therefore, m is never suitable again, and m must be increasing, so d is strictly increasing.

    */
    }

    def q9ManualLog3(x:Int) : Int = {
        assert(x>=0)
        if (x==0) return 0
        var answer = 0
        var total = 1
        // Invariant: answer > 0, total > 0, total = 3 ^ answer
        while (total <= x) {
            answer += 1
            total *= 3
        }
        answer - 1
    }
    
    def q10PolynomialEval(coefficients: Array[Double], x: Double) : Double = {
        var idx = coefficients.length
        var total = 0.0
        // Invariant: total >= 0, coefficients.length - 1 >= idx >= 0, total = ((coefficients(coefficients.length - 1)x + coefficients(coefficients.length - 2))x + ... )x + coefficients(idx)
        while (idx > 0) {
            idx -= 1
            total = x * total + coefficients(idx)
        }
        total
    }
}