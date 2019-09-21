##output: 6

fun main() -> Unit {
    let f = fun(x: Int, y: Int) -> Int { y + x; };
    print(f(1, 5));
}
