int f1(int a) {
  return a + 1;
}

int f2(int a) {
  return f1(a) + 1;
}

int f3(int a) {
  print(a);
  if (a > 10) {
    return f3(a - 1);
  }
  else {
    return 2;
  }
}

int f4(int a) {
  {
    return a;
  }
}

void v() {
  int x = 100;
  print(x);
  return;
  print(x + 1);
}

string s(boolean b, string text) {
  if (b) {
    return text;
  }
  return "nothing";
}

int main() {
  int x = 42;
  print(f1(x));
  print(f2(x));
  print(f3(x));
  v();
  string w = s(true, "some text");
  print(w);
  return 20;
}
