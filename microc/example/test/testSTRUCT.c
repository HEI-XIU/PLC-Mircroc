struct test {
  int i;
  char c;
  int a[3];
};
void main() { 
  struct test t;
  struct test t2;
  t.i = 3;
  t.c = 'b';
  t2.i = 7;
//   print t.i;
//   print t.c;
//   print t2.i;
  printf("%d", t.i);
  printf("%c", t.c);
  printf("%d", t2.i);
//   printf("%s", t.s);

}