##shouldNotTypecheck

effect myEffect1() -> Unit
effect myEffect2() -> Unit

fun eff1Func() : myEffect1 -> Unit {
    myEffect2();
}

fun main() : IO -> Unit {}
