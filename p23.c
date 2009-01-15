/* p23.c
 *
 * Project Euler
 *
 * http://projecteuler.net/index.php?section=problems&id=23
 *
 * Problem 23
 * 02 August 2002
 *
 * A perfect number is a number for which the sum of its proper divisors is
 * exactly equal to the number. For example, the sum of the proper divisors of
 * 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
 *
 * A number whose proper divisors are less than the number is called deficient
 * and a number whose proper divisors exceed the number is called abundant.
 *
 * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
 * number that can be written as the sum of two abundant numbers is 24. By
 * mathematical analysis, it can be shown that all integers greater than 28123
 * can be written as the sum of two abundant numbers. However, this upper limit
 * cannot be reduced any further by analysis even though it is known that the
 * greatest number that cannot be expressed as the sum of two abundant numbers
 * is less than this limit.
 *
 * Find the sum of all the positive integers which cannot be written as the sum
 * of two abundant numbers.
 *
 * Answer:
 *      4179871
 */


#include <stdio.h>
#include <stdlib.h>


int pwr(int x, int po)
/*         po
 * return x
 *
 * (power po of the x)
 */
{
        int r = 1;
        while (po > 0) {
                r *= x;
                po--;
        }
        return r;
}


int S(int x)
/* sigma function
 * return sum of proper divisors of the x
 *
 * see:
 *   http://mathworld.wolfram.com/DivisorFunction.html
 *   http://primes.utm.edu/glossary/page.php?sort=SigmaFunction
 */
{
        int X = x;
        int s = 1;
        int d = 2; /* first prime */
        while (X != 1) {
                if ((X % d) == 0) {
                        int po = 0;
                        while ((X % d) == 0) {
                                X /= d;
                                po++;
                        }
                        s *=(pwr(d, po + 1) - 1)/(d - 1);
                }
                d++;
        }
        return s - x; /* exclude x itself from the sum 
                       * to take into account _proper_ divisors only
                       */
}


#define LIMIT 28123


int main(void)
{
        int *AN = (int*)malloc(sizeof(int) * LIMIT); /* list of abundant numbers */
        int N = 0;  /* number of abundant numbers */
        int x = 12; /* first abundant number */
        int *flag = (int*)malloc(sizeof(int)*(LIMIT+1));
        int i, j, s;

        printf("making list of abundant numbers\n");
        while (x < LIMIT) {
                if (S(x) > x)
                        AN[N++] = x;
                x++;
        }
        printf("number of abundant numbers is %d\n", N);

        printf("making list of numbers to sum up\n");
        for (i = 0; i <= LIMIT; i++)
                flag[i] = 0;
        for (i = 0; i < N; i++) {
                for (j = i; j < N; j++) {
                        x = AN[i] + AN[j]; /* the sum of two abundant numbers */
                        if (x <= LIMIT)
                                flag[x] = 1;
                }
        }

        printf("summing up\n");
        s = 0;
        for (i = 0; i <= LIMIT; i++)
                if (flag[i] == 0)
                        s += i;

        printf("%d\n", s);

        free(AN);
        free(flag);

        return 0;
}


/* end of file */
