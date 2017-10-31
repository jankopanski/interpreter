int main() {
  new int[10];
  int[] t1;
  t1 = new int[4];
  print(t1);
  t1[2] = 42;
  print(t1);
  t1[2];
  print(t1[2]);
  int[] t2 = new int[5];
  t2[1 + 1] = t1[1 + 1];
  print(t2);
  string[] t3 = new string[3];
  t3[0] = "test";
  t3[1] = "of";
  t3[2] = "arrays";
  print(t3);
  <int, int>[] t4 = new <int, int>[2];
  t4[1] = <(3,4)>;
  t4[0] = <(1,2)>;
  print(t4);
  return 0;
}
