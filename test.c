int
recursive_fib(int n){
	int n1, n2, r;
	if(n==0 || n==1){
		return n;
	};
	n1=n-1;
	n2=n-2;
	r=recursive_fib(n1) + recursive_fib(n2);
	return r;
}

main(){
	int n,i,temp;
	read(n);
	for(i=0;i<n;i++){
		temp = recursive_fib(i);
		print(temp);
	};
}
