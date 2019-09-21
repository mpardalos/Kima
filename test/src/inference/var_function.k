##output: 6

fun main() -> Unit {
    var f = fun(x: Int, y: Int) -> Int { y + x; };
    print(f(1, 5));
}
