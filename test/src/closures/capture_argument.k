##output: abc

fun printer(s: String) -> () -> Unit {
    fun() -> Unit {
        print(s);
    }
}

fun main() -> Unit {
    printer("abc")();
}
