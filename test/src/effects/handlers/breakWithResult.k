##output: 0

effect fail() -> Unit

fun main() : IO -> Unit {
    let result = handle {
        fail();
        10;
    } with {
        fail() -> Unit {
            break 0;
        }
    };

    print(result);
}
