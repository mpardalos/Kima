##shouldNotTypecheck

effect readInt() -> Int
effect readOtherInt() -> Int

fun proc() : { readInt, readOtherInt } -> Int {
    readInt() + readOtherInt();
}

fun main() : IO -> Unit {
    let result: Int =
        handle proc() {
            readInt() -> Int { 3 }
        };
    print(result);
}
