void main(int n) { 
  int i; 
  i=0; 
  while (i < n) { 
    if (i == 1) {
        i = i + 1;
        continue;
      }
    print i; 
    i=i+1;
  }
}