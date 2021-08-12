##output: red

data Color {
     Red, Green, Blue
}

fun main() : IO -> Unit {
    let c : Color = Red;
    match (c) {
        Green() { print("green"); }
        Blue() { print("blue"); }
        Red() { print("red"); }
    };
}
