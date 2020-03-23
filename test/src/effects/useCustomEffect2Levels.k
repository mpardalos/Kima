effect myEffect() -> Unit

fun myEffectfulFunc1() : myEffect -> Unit {
    myEffect();
}

fun myEffectfulFunc2() : myEffect -> Unit {
    myEffectfulFunc2();
}

fun main() : IO -> Unit {}
