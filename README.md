# Kima [![pipeline status](https://gitlab.com/michalis_pardalos/Kima/badges/master/pipeline.svg)](https://gitlab.com/michalis_pardalos/Kima/commits/master)

A Programming Language with static types and (currently WIP) algebraic effects.

[Language website](https://kima.xyz)

## Getting started

### Installing

``` sh
git clone https://gitlab.com/michalis_pardalos/Kima.git
cd Kima
stack install
```

### Usage

You can run a file with
``` sh
kima run example.k
```

or start the REPL with

``` sh
kima repl
```

any other options should be documented in 

``` sh
kima --help
```

## Examples

Let's get the basic Hello World out of the way

```
fun main() -> Unit {
    print("Hello World!");
}
```

We can define our string as a local variable too:

```
fun main() -> Unit {
    let output: String = "Hello World!";
    print(output);
}
```

But there's no need to say the string is a String. Type inference takes care of
that!

```
fun main() -> Unit {
    let output = "Hello World!";
    print(output);
}
```

What if we changed out mind about the string we want to print?

```
fun main() -> Unit {
    let output = "Hello World!";
    output = "Hello Everyone!"
    print(output);
}
```

Oops! That's an error. `let` creates immutable variables. Use `var` for mutable
variables.

```
fun main() -> Unit {
    var output = "Hello World!";
    output = "Hello Everyone!"
    print(output);
}
```

That's better! We can also make a function to return a custom greeting

```
fun greet(name: String) -> String {
    "Hello " + name;
}

fun main() -> Unit {
    let output = greet("everyone!");
    print(output);
}
```

As you can see, functions return the value of the last statement in their body.

Functions can also take functions as arguments or return them:

```
fun adder(x: Int) -> (Int) -> Int {
    fun(y: Int) -> Int {
        x + y;
    }
}

fun main() -> {
    let add_3 = adder(3);
    print("3 + 5 = ");
    print(add_3(5));
}
```

You can define data types like so:

```
data Email(name: String, server: String, tld: String)

fun to_string(email: Email) -> String {
    email.name + "@" + email.server + "." + email.tld;
}

fun main() -> Unit {
    print(to_string(Email("john.smith", "example", "com")));
}
```

And more features are coming soon!

## TODO

* [x] Builtin types
* [x] Higher-order functions
* [x] Product types (records)
* [x] Type checking
* [x] Type inference (bidirectional typing takes care of both)
* [x] Control flow (if, while)
* [ ] Pattern matching
* [ ] Effect system
* [ ] Sum types
* [ ] Polymorphic types
* [ ] Source position annotations (better error messages)
