# Multiple assignments

fun main() : IO -> Unit {
    var b: String = "hello";
    var i: Int = 0;
    while (i < 100) {
        b = b + "!";
        i = i + 1;
    }
}