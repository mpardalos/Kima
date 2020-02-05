##output: done

fun f(n: Int) -> Unit {
    if (n > 0) { f(n-1); }
    else {}
}

fun main() => IO -> Unit {
    f(10);
    print("done");
}
