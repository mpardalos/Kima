##output: 6

fun add_curried(x1: Int) -> (Int) -> Int {
    fun (x2: Int) -> Int { x1 + x2 }
}

fun main() -> Unit {
    print(add_curried(2)(3));
}
