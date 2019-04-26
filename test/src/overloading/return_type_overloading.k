##output: 1
##output: one
fun f() -> Int { 1 }
fun f() -> String { "one" }

fun main() -> Unit {
    let x: Int = f();
    let y: String = f();

    print(x);
    print(y);
}
