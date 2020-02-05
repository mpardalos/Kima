##output: abc

fun printer(s: String) -> () => IO -> Unit {
    fun() => IO -> Unit {
        print(s);
    }
}

fun main() => IO -> Unit {
    printer("abc")();
}
