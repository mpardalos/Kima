##output: Just(Red)

data Color { Red, Green, Blue }
data MaybeColor { Just(Color), Nothing }

fun main() : IO -> Unit {
    let c : MaybeColor = Just(Red);
    match (c) {
        Just(Red()) { print("Just(Red)"); }
        Just(c) { print("Just(other)"); }
        Nothing { print("Nothing"); }
    };
}
