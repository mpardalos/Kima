##output: Just(other)

data Color { Red, Green, Blue }
data MaybeColor { Just(Color), Nothing }

fun main() : IO -> Unit {
    let c : MaybeColor = Just(Blue);
    match (c) {
        Just(Red()) { print("Just(Red)"); }
        Just(c) { print("Just(other)"); }
        Nothing { print("Nothing"); }
    };
}
