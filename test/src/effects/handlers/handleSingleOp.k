##focused
##output: 2

effect readInt() -> Int

fun readAndAdd1() : readInt -> Int { readInt() + 1; }

fun main() : IO -> Unit {
    let result: Int =
        handle readAndAdd1() {
            readInt() -> Int { 1 }
        };
    print(result);
}
