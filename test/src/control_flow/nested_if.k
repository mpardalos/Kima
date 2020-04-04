##output: second_case

fun main() : IO -> Unit {
    if False {
        print("first_case");
    } else if True {
        print("second_case");
    } else {
        print("third_case");
    }
}
