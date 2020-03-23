##output: abc

fun capturer(s: String) -> () -> String {
    fun() -> String { s; }
}

fun main() : IO -> Unit {
    let s = capturer("abc")();
    print(s);
}
