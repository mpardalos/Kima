data Date(day: Int, month: Int, year: Int)
data Event(date: Date, name: String)

fun main() => IO -> Unit {
    var e: Event = Event(Date(1, 10, 2018), "anniversary");
    e.date.month = 5;
    print(e.date.month)
}
