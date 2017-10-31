int main() {
  <int, boolean> t1;
  <(42)>;
  <(42, true)>;
  <int, boolean> t2 = <(42, true)>;
  <int, boolean, string> t3 = <(42, true, "text")>;
  print(t3);
  t3<(1)>;
  print(t3<(1)>);
  <int, <int>> t4 = <(1, <(2)>)>;
  <int, <<int, int>, int, <int>>, int> t5 = <(1, <(<(4, 5)>, 3, <(6)>)>, 2)>;
  print(t5);
  return 0;
}
