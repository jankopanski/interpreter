int main() {
  int[[int]] x;
  int[[string]] y = new int[[string]];
  y[["a"]] = 1;
  int n = y[["a"]];
  boolean b1 = has y[["a"]];
  int m = del y[["a"]];
  return 0;
}
