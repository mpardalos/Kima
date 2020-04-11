##focused
##output: ABCD

effect fail() -> Unit

fun proc() : {IO, fail} -> Unit {
    print("B");
    fail();
    print("~NEVER~");
}

fun main() : IO -> Unit {
    print("A");
    handle proc() {
        fail() -> Unit {
            print("C");
            break;
            print("~NEVER~");
        }
    };
    print("D");
}
