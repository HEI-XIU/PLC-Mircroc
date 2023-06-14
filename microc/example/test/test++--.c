//无法实现
void main(int n) {
   int i;
   int i=0;
//  printf("%d",i++) ;
   i=0;
   do {
        i++;
       printf("%d",i) ;
   } 
   while(i<n); 
   i=0;
   do {
        ++i;
       printf("%d",i) ;
   }  
   while(i<n);
   i=20;
   do {
        i--;
       printf("%d",i) ;
   } 
   while(i>n); 
   i=20;
   do {
        --i;
       printf("%d",i) ;
   }  
   while(i>n);
}