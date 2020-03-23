##shouldNotTypecheck

effect myEffect() -> Unit

fun myPureFunc() : pure -> Unit {
    myEffect();
}

fun main() : IO -> Unit {}
