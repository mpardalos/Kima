##output: Blue

data Color { Red, Green, Blue }
data MaybeColor { Just(Color), Nothing }

fun showColor(c: Color) -> String {
    match (c) {
        Red() { "Red" }
        Green() { "Green" }
        Blue() { "Blue" }
    }
}

fun main() : IO -> Unit {
    let mc : MaybeColor = Just(Blue);
    match (mc) {
        Just(c) { print(showColor(c)); }
        Nothing { print("Nothing"); }
    };
}
