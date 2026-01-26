object Maps {
    def mapTest() : Unit = {
        var capital = Map("US" -> "Washington", "France" -> "Paris")
        capital += ("Japan" -> "Tokyo")
        println(capital("France"))
    }

    def main(args: Array[String]) : Unit = {
        mapTest()
    }
}   