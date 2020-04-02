fun overloaded() -> Unit {}
fun overloaded(n: Int) -> Unit {}
fun overloaded(n: Int, z: Int) -> Unit {}

fun main() : IO -> Unit {
    overloaded();
    overloaded(1);
    overloaded(1, 1);
}
