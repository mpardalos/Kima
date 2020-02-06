##output: 5a

fun id(x: Int) -> Int { x; }
fun id(s: String) -> String { s; }

fun main() : IO -> Unit {
    let n = id(5);
    let s = id("a");

    print(n);
    print(s);
}
