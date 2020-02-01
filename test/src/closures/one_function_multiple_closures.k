##output: 5
##output: 6
##output: 10

fun make_accumulator() -> (Int) -> Int {
    var val: Int = 0;
    fun(n: Int) -> Int {
          val = val + n;
          val;
    }
}

fun main() -> IO Unit {
    let acc1: (Int) -> Int = make_accumulator();
    let acc2: (Int) -> Int = make_accumulator();

    print(acc1(5)); # 5
    print(acc1(1)); # 6
    print(acc2(10)); # 10
}
