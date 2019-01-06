##shouldFailWith: AssignToConst

fun main() -> Unit {
    let b: String = "hello";
    b = "world";
}