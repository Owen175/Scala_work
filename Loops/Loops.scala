object Loops {
    def whl(args: String*) : Unit = {
        var i = 0
        while (i < args.length) {
            if (i != 0) {
                print(" ")
            }
            print(args(i))
            i+=1
        }
        println()
    }
    def forEach(args: String*) : Unit = args.forEach((arg: String)=>println(arg))
    def alternativeForEach(args: String*) : Unit = args.forEach(println)  /* if there is one argument for the function, just pass the function */
    def fr(args: String*) = {
        for (arg <- args) {
            println(arg)
        }
    }
    def main(args: Array[String]) : Unit = {
        whl(args)
        forEach(args)
        alternativeForEach(args)
        fr(args)
    }
}