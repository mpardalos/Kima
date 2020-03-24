##shouldNotTypecheck

fun notReallyPure(x: Int) : pure -> Int {
    print(x);
    x + 1;
}

fun main() : IO -> Unit {
    print("hello");
}
