##output: eff1eff2

effect eff1() -> Unit
effect eff2() -> Unit

fun main() : IO -> Unit {
    handle eff1() {
        eff1() -> Unit { print("eff1"); eff2(); }
        eff2() -> Unit { print("eff2"); }
    }
}
