##output: 1
##output: 2

fun make_f() -> () -> Unit {
    var a: Int = 0;
    fun() -> Unit {
        a = a + 1;
        print(a);
    };
}

fun main() => IO -> Unit {
    let f: () -> Unit = make_f();
    f();
    f();
}
