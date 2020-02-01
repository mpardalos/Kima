fun main() -> IO Unit {
    print(typeof(5));
    print(typeof("abc"));
}

fun typeof(n: Int) -> String {
    "Integer"
}

fun typeof(n: String) -> String {
    "String"
}