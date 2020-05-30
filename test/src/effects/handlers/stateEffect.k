##output: 1

effect getState() -> Int
effect setState(newVal: Int) -> Unit
effect State { getState, setState }

fun main() : IO -> Unit {
    var stateVal: Int = 0;
    handle {
        let initial = getState();
        setState(50);
        setState(initial + 1);
    } with {
        getState() -> Int { stateVal; }
        setState(newVal: Int) -> Unit {
            stateVal = newVal;
        }
    };
    print(stateVal);
}
