##shouldNotTypecheck

effect myEffect(n: Int) -> Unit

fun myEffectfulFunc() : myEffect -> Unit {
    myEffect("hi");
}

fun main() : IO -> Unit {}
