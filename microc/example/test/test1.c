struct test2 {
        int a;
        float b;

} ;
struct test1 {
        int a;
        float f;
        int arr[100];
        char c;
        bool b;
        string s;

} ;

    
int main(int n) {
  int test3 ;
  test3 = 23;
  struct test1 t;
  struct test2  t2;
  t.a = 145;
  t.f = 12.3;
  t.s = "asasasas";
  t.arr[2]= 1;
  t.arr[5]= 2;
  t.arr[6]= 5;
  t2.a = 12121212;
  t.c = '1';
  t.b = false;
  int test ;
  test= t.a+t2.a;

  printf("%c",t.c);
  printf("%d",t.b);
  printf("%d",t2.a);
  printf("%s",t.s);
  printf("%d",t.a);
  printf("%f",t.f);
  printf("%d",t.arr[6]);
  printf("%d",test3);
  printf("%d",test);

 
}