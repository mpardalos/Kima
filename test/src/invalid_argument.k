##shouldFailWith: CantUnifyCall

fun main() -> Unit {
    takes_int("hi");
}

fun takes_int(n:Int) -> Unit {}