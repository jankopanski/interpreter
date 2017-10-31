int fun() {
  int tr1(int y) {
    int tr2(int y) return y + 1;
    return tr2(y + 1);
  }
  return tr1(3);
}

int main() {
  int x = 0;
  print("f1");
  int f1(int y) return y + 1;
  print(f1(x));

  print("f2");
  int f2(int y) {
    return y + 2;
  }
  print(f2(x));

  print("f3");
  int f3(int y) {
    int a = 3;
    return y + a;
    a = 4;
  }
  print(f3(x));

  print("f4");
  int f4(int y) {
    int x = 42;
    return y + 4;
  }
  print(f4(x));
  print(x);

  print("r1 r2");
  int r1(int y) {
    int x = y + 1;
    int r2(int y) {
      int x = y + 1;
      print(x);
      return x;
    }
    int z = r2(x);
    print(x);
    return z;
  }
  print(r1(x));

  print("g");
  int g(int x) {
    if (x < 5) {
      print(x);
      return g(x + 1);
    }
    return 13;
  }
  print(g(0));

  print("fun");
  print(fun());
  return 0;
}
