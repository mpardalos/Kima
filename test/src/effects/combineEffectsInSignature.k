effect myEffect1() -> Unit
effect myEffect2() -> Unit

fun myEffectfulFunc() : { myEffect1, myEffect2 } -> Unit {
    myEffect1();
    myEffect2();
}

fun main() : IO -> Unit {}
