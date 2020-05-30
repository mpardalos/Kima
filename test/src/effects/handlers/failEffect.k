##output: ABCD

effect fail() -> Unit

fun main() : IO -> Unit {
    print("A");
    handle {
        print("B");
        fail();
        print("~NEVER~");
    } with {
        fail() -> Unit {
            print("C");
            break;
            print("~NEVER~");
        }
    };
    print("D");
}
