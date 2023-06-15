int fact(int i){
    if(i == 1){
        return 1;
    }else{
        return i * fact(i - 1);
    }
}
int main(){
    int n;
    n=4;
    printf("%d", fact(n));
}