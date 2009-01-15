/*===================================================================*/
/* C program for distribution from the Combinatorial Object Server.  */
/* Generate combinations in lexicographic order. This is             */
/* the same version used in the book "Combinatorial Generation."     */
/* The program can be modified, translated to other languages, etc., */
/* so long as proper acknowledgement is given (author and source).   */  
/* Programmer: Joe Sawada, 1997.                                     */
/* The latest version of this program may be found at the site       */
/* http://theory.cs.uvic.ca/inf/comb/CombinationsInfo.html            */
/*===================================================================*/

#include <stdio.h>

int  n,k;
int  a[100];   /* The string */

void PrintIt() {

	int i;
	for (i=1; i <= n; i++) printf( "%d", a[i] );
	printf("\n");
} 

void Comb(int j, int m) {

	if (j > n) PrintIt();
	else {
		if (k-m < n-j+1) {
			a[j] = 0; Comb(j+1, m);
		}
		if (m<k) {
			a[j] = 1; Comb(j+1, m+1);
		}
	}
}

int main (int argc, char *argv[]) {

/*	printf( "Enter n,k: " );  scanf( "%d %d", &n,&k); */
        n = atoi(argv[1]);
        k = atoi(argv[2]);

	if (n<=0) return 1;

	printf( "\n" );
	Comb(1,0);
	printf( "\n" );
        return 0;
}



