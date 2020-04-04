##output: 1

effect getState() -> Int
effect setState(newVal: Int) -> Unit
effect State { getState, setState }

fun proc() : State -> Unit {
    let initial = getState();
    setState(50);
    setState(initial + 1);
}

fun main() : IO -> Unit {
    var stateVal: Int = 0;
    handle proc() {
        getState() -> Int { stateVal; }
        setState(newVal: Int) -> Unit {
            stateVal = newVal;
        }
    };
    print(stateVal);
}
