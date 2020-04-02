effect myEffect() -> Unit
effect myEffect(n: Int) -> Int

fun myEffectfulFunc() : myEffect -> Unit {
    myEffect();
}

fun main() : IO -> Unit {}
