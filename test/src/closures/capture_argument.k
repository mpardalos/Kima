##output: abc

fun printer(s: String) -> () -> Unit {
    fun() -> Unit {
        print(s);
    }
}

fun main() -> IO Unit {
    printer("abc")();
}
