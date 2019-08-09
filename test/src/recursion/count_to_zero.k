##output: done

fun f(n: Int) -> Unit {
    if (n > 0) { f(n-1); }
    else {}
}

fun main() -> Unit {
    f(10);
    print("done");
}
