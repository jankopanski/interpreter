int main() {
  new string[[int]];
  string[[int]] m1 = new string[[int]];
  m1[[1]] = "one";
  m1[[6]] = "six";
  m1[[9]] = "nine";
  print(m1);
  m1[[1]];
  print(m1[[1]]);
  boolean b1 = has m1[[6]], b2 = has m1[[7]];
  print(b1, b2);
  del m1[[9]];
  print(m1);
  return 0;
}
