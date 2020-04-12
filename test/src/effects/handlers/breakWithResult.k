##output: 0

effect fail() -> Unit

fun proc() : {IO, fail} -> Int {
    fail();
    10;
}

fun main() : IO -> Unit {
    let result = handle proc() {
        fail() -> Unit {
            break 0;
        }
    };

    print(result);
}
