##output: red
##focused

data Color {
     Red, Green, Blue
}

fun main() : IO -> Unit {
    let c : Color = Red;
    match (c) {
        Red() { print("red"); }
        Green() { print("green"); }
        Blue() { print("blue"); }
    };
}
