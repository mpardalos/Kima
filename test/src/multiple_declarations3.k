##shouldFailWith: NameShadowed

fun main() -> Unit {
    var a: Int = 5;
    let a: Int = 3;
}