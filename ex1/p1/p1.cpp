#include<stdio.h> 
#include<vector> 

using namespace std ;

typedef vector< vector<int> > matrix; 


// Factorial implementation
long long factorial (int n) { 
	long long res = 1; 

	for (int i=2; i<=n; i++) 
		res *= i ; 

	return res; 
}


// Matrix multiplication implementation
void mat_mult( matrix &a, matrix &b ,matrix &c ) { 
	
	for(int i=0; i< (int) a.size(); i++) { // a.size == n
		for(int j=0; j< (int) b[0].size() ; j++) {  // b[0].size() == p
			for (int k=0; k< (int) b.size(); k++)   // b.size() == m
				c[i][j] += a[i][k] * b[k][j] ; 
		}
	}
	
}


void print_matrix(matrix &a) {  // second dimension deduced

	for(auto row : a) {
		for(auto el : row) 
			printf("%d ", el) ;
		printf("\n"); 
	}

}


void read_matrix(matrix &a) { 

	for(int i=0 ;i< (int) a.size(); i++)  {
		for(int j=0; j< (int) a[0].size(); j++)  
			scanf("%d",&a[i][j]); 
	}

}


// Matrix is initializated with size to use
void handle_size(matrix &a, int n, int m) { 
	a.resize(n); 

	for(int i=0 ;i<n; i++) 
		a[i].resize(m); 
}


// test matrix product
void test_matrix_product() { 
	int n, m, p; 
	matrix a,b,c ; 

	printf("Matrix product\n"); 

	printf("Give me n, m and p dimensions: ");
	scanf("%d %d %d", &n, &m, &p);

	// Give matrices propper sizes
	handle_size(a,n,m); 
	handle_size(b,m,p); 
	handle_size(c,n,p); 

	// read input matrices
	printf("Give me first input matrix:\n"); 
	read_matrix(a); 
	printf("Give me second input matrix:\n"); 
	read_matrix(b); 

	// multiply them 
	mat_mult(a,b,c) ;
	
	printf("Product matrix is:\n") ;
	print_matrix(c); 
}

// test factorial
void test_factorial() { 
	long long n; 
	printf("\nFactorial\n"); 

	printf("Give me a number: " ) ; 
	scanf("%lld",&n); 

	printf("factorial of %lld is: %lld\n", n, factorial(n)); 
}


int main() { 
	test_matrix_product(); 
	test_factorial();
}
