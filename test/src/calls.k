fun main() -> Unit {
    hello_sayer()();
}

fun say_hello() -> Unit {
    print("Hello");
}

fun hello_sayer() -> () -> Unit {
    say_hello;
}