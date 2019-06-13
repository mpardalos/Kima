##output: 1
##output: one

fun f() -> () -> Int { fun () -> Int { 1 } }
fun f() -> () -> String { fun () -> String { "one" } }

fun main() -> Unit {
    let x: Int = f()();
    let y: String = f()();

    print(x);
    print(y);
}
