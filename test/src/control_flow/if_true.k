##output: if_block

fun main() => IO -> Unit {
    if (True) {
        print("if_block");
    } else {
        print("else_block");
    }
}