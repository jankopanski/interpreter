int main() {
  int s = 0, n = 42;

  int i = 0;
  while (i <= n) {
    s = s + i;
    i++;
  }

  s = 0;
  for j = 0 to n do {
    s = s + j;
  }

  s = 0;
  for j = n downto 0 do {
    s = s + j;
  }
  return 0;
}
