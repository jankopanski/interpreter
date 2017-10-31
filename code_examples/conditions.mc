int main() {
  int a = 42;
  if (a < 100) {
    print(1);
  }
  else {
    print(2);
  }

  if (true) print(3);

  if (true == false) {
    print(4);
  }
  else if (1 <= 0) {
    print(5);
  }
  else if ("aaa" > "aba") {
    print(6);
  }
  else {
    print(7);
  }

  if (false) print(8);
  else if(true) print(9);
  else print(10);
  return 0;
}
