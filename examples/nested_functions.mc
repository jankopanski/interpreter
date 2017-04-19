int foo() {
  int bar() {
    int baz() {
      return 42;
    };
    return baz();
  };
  int x = 1 + bar();
  return x;
}

int main() {
  foo();
}
