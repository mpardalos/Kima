##output: 8

effect readInt() -> Int
effect readOtherInt() -> Int
effect readYetAnotherInt() -> Int

fun main() : IO -> Unit {
    let result: Int =
        handle (readInt() + readOtherInt()) {
            readInt() -> Int { 3 }
            readOtherInt() -> Int { 5 }
            readYetAnotherInt() -> Int { 5 }
        };
    print(result);
}
