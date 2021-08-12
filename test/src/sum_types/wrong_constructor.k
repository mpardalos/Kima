##shouldNotTypecheck

data Color { Red, Green, Blue }

data Direction { Up, Down, Left, Right }

fun main() : IO -> Unit {
    let c : Color = Red;
    match (c) {
        Up() { print("up"); }
        Down() { print("down"); }
        Left() { print("left"); }
        Right() { print("right"); }
    };
}
