##shouldNotTypecheck

fun shadows() -> () -> Unit {
    let s: String = "outer";

    let function: () -> Unit = fun() -> Unit {
        let s: String = "inner";
        s;
    };

    function();
}

fun main() -> Unit {
    print(shadows())
}
