data Date(day: Int, month: Int, year: Int)

fun main() : IO -> Unit {
    var d: Date = Date(1, 10, 2018);
    d.month = 5;
}
