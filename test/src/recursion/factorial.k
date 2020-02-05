##output: 120

fun fac(n: Int) -> Int {
    if (n == 0) { 1 }
    else { n * fac(n-1)}
}

fun main() => IO -> Unit {
    print(fac(5))
}
