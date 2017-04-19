int fun(boolean b) {
  if (b == true) return 1;
  else return 2;
}

int main() {
  int x;
  x = 1;
  int y = fun(false);
  boolean u = true, v;
  y++;
  if (!u) {
    int z = x * y + x / y % x;
  }
  else if (x < 3) {
    return x;
  }
  return 0;
}
