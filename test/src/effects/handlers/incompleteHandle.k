##shouldNotTypecheck

effect readInt() -> Int
effect readOtherInt() -> Int

fun main() : IO -> Unit {
    let result: Int =
        handle (readInt() + readOtherInt()) {
            readInt() -> Int { 3 }
        };
    print(result);
}
