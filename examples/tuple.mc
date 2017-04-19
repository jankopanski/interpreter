int main() {
  <int, boolean, string> x = <(1, true, "x")>;
  <int, <boolean, string>> y = <(2, <(false, "y")>)>;
  int u = y<(2)>;
  boolean b = u<(1)>;
  <<<int>>> z = <(<(<(42)>)>)>;
  <<int>> v = z<(0)>;
  v<(0)>;
  return 0;
}
