##shouldNotTypecheck

fun main() : IO -> Unit {
    let result: Int =
        handle 0 {
            someEffect() -> Unit {}
        };
}
