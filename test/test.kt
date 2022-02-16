class Foo {
    fun error() {
        assert(false);
    }
}

fun main() {
    val test = Foo();
    test.error();
}
