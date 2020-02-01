##output: 6

fun main() -> IO Unit {
    var f = fun(x: Int, y: Int) -> Int { y + x; };
    print(f(1, 5));
}
