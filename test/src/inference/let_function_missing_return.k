##output: 6

fun main() => IO -> Unit {
    let f = fun(x: Int, y: Int) { y + x; };
    print(f(1, 5));
}
