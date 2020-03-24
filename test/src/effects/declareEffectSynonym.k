effect myEffect1() -> Unit
effect myEffect2() -> Unit

effect MyCombinedEffect {
    myEffect1,
    myEffect2
}

fun main() : IO -> Unit {}
