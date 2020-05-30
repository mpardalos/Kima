##output: 2

effect readInt() -> Int

fun main() : IO -> Unit {
    let result: Int =
        handle (readInt() + 1) {
            readInt() -> Int { 1 }
        };
    print(result);
}
