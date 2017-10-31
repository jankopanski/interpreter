int main() {
  string s1 = "text", s2 = "42";
  int n = 5;
  print(s1, s2);
  string con1 = concatStr(s1, s2);
  print(con1);
  string ns = intToStr(n);
  string con2 = concatStr(s1, s2, ns);
  print(con2);
  print(n + strToInt(s2));
  return 0;
}
