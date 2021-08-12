##output: 5

data MaybeInt { Just(Int), Nothing }

fun main() : IO -> Unit {
    let m : MaybeInt = Just(5);
    match (m) {
          Nothing() { print("Nothing"); }
          Just(n) { print(n); }
    };
}
