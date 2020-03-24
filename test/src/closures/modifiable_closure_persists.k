##output: 1
##output: 2

fun make_f() -> () -> Int {
    var a: Int = 0;
    fun() -> Int {
        a = a + 1;
        a;
    };
}

fun main() : IO -> Unit {
    let f: () -> Int = make_f();
    print(f());
    print(f());
}
