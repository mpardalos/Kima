##output: 2

fun main() -> Unit {
    var a: Int = 0;

    let function:() -> Unit = fun() -> Unit {
        a = a + 1;
    };

    function();
    function();

    print(a);
}
