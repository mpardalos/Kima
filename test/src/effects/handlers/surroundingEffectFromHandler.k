##output: ABC

effect op() -> Unit

fun main() : IO -> Unit {
    handle op() {
        op() -> Unit {
             print("A");
             print("B");
             print("C");
        }
    }
}
