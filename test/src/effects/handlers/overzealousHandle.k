##output: 8

effect readInt() -> Int
effect readOtherInt() -> Int
effect readYetAnotherInt() -> Int

fun proc() : { readInt, readOtherInt } -> Int {
    readInt() + readOtherInt();
}

fun main() : IO -> Unit {
    let result: Int =
        handle proc() {
            readInt() -> Int { 3 }
            readOtherInt() -> Int { 5 }
            readYetAnotherInt() -> Int { 5 }
        };
    print(result);
}
