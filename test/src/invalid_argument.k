##shouldNotTypecheck

fun takes_int(n:Int) -> Unit {}

fun main() => IO -> Unit {
    takes_int("hi");
}
