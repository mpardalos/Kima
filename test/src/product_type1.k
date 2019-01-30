##output: john.smith@example.com

data Email(name: String, server: String, tld: String)

fun to_string(email: Email) -> String {
    email.name + "@" + email.server + "." + email.tld;
}

fun main() -> Unit {
    print(to_string(Email("john.smith", "example", "com")));
}
