##focused
##output: 1

effect readInt() -> Int

fun readAndAdd1() : readInt -> Int { readInt() + 1; }

fun main() : IO -> Unit {
    let result: Int =
        handle readAndAdd1 {
            readInt() { 1 }
        };
    print(result);
}
