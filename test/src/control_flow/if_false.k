##output: else_block

fun main() => IO -> Unit {
    if (False) {
        print("if_block");
    } else {
        print("else_block");
    }
}