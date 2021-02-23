#include<stdio.h>  
#include<stdlib.h> 
#include<time.h> 


/* 
 * Iterative derivation from tail recursive implementation of f_47
 */
long long iterative_f_47 ( long long n ) { 
	long long a, b, c, d ; 
	int ind; 

	/* Notice the same group of assignations. */
	ind = (n / 7) -3 ;

	a = n % 7 ; 
	b = a + 7; 
  	c = b + 7; 
 	d = c + 7; 	

	int temp = d; /* Except for a temporary variable to swap. */

	/* What acted as a decremental index for the tail recursion */
	/* is now the upper bound of the iteration. */
	for (int i=0; i < ind ; i++) { 
		d = a + b + c + d; 
		a = b; 
		b = c; 
		c = temp ; 
		temp = d; 
	}

	return d; 
}


/* 
 * Auxiliar functino for tail recursive implementation
 * It is also tail recursive
*/
long long helper_f_47 ( int i, long long a, long long b, long long c, long long result) { 
	
	if (i == 0) 
		return result; 

	return helper_f_47 ( i-1, b, c, result, result + a + b + c) ; 
}

/*
 * Tail recursive implementation of f_47 function
 */
long long tail_f_47 ( long long n ) { 
	
	long long a, b, c, d; 
	int ind; 

	if ( 0 <= n && n < 28 ) 
		return n ; 

	ind = (n/7) - 3; 	

	// establish base cases
	a = n % 7 ; 
	b = a + 7; 
  	c = b + 7; 
 	d = c + 7; 	

	return helper_f_47 (ind, a, b, c, d) 	;
}


/*
 * Recursion explicitly transformed from f_47 formula
 */
long long f_47 ( long long n ) { 

	if ( 0 <= n && n < 28 ) 
		return n ; 

	return f_47 (n-7) + f_47 (n-14) + f_47 (n-21) + f_47 (n-28) ; 
}

int main() { 

}
