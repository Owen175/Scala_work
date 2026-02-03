object Quicksorts {
    def main(args: Array[String]) : Unit = {
        val arr = Array(5, 3, 2, 4, 5, 6, 7, 2, 3, 4)
        quicksortInPlace(arr, 0, arr.length)
        for (a <- arr) print(a + " ")
    }

    def quicksortInPlace(arr: Array[Int], start: Int, end: Int) : Unit = {
        val len = end - start
        if (len <= 1) return
        var l = start + 1; var r = end
        var piv = arr(start)
        // Invariant: a[start+1 .. l) < piv = a(start) <= a[r..end) ^ start < l <= r <= end ^ a[0..start) = a_0[0..start) && a[end-N) = a_0[r..N) ^ a[start .. end) is a permulation of a_0[start..end)
        while (l < r) {
            if (arr(l) <= piv) {
                l += 1
            } else {
                val temp = arr(l)
                arr(l) = arr(r-1)
                arr(r-1) = temp
                r -= 1
            }
        }
        arr(start) = arr(l-1)
        arr(l-1) = piv
        quicksortInPlace(arr, start, l-1)
        quicksortInPlace(arr, l, end)
    }

}