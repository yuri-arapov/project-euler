/* 27 of December, 2007
 *
 * Yuri Arapov <yuridichesky@gmail.com>
 *
 * Project Euler
 *
 * Problem 4
 * http://projecteuler.net/index.php?section=problems&id=4
 *
 * A palindromic number reads the same both ways. The largest
 * palindrome made from the product of two 2-digit numbers is
 * 9009 = 91  99.
 *
 * Find the largest palindrome made from the product of two
 * 3-digit numbers.
 *
 * Answer:
 *      906609
 *
 * Done.
 */

#include <stdio.h>


int is_palindromic(int n)
/* FIXME: */
{
        int i;
        int nn;
        int tens = 10;
        int digits = 1; /* number of digits */

        int left, left_10_i, left_10_i_1;
        int right, right_10_i, right_10_i_1;

        /* determine number of digits */
        nn = n;
        while ((nn = nn / 10) > 0) {
                digits++;
                tens *= 10;
        }

        if (digits == 1)
                return 1; /* 1-digit number is palindromic */

        /* ok, there are 'digits' digits in the number n */

        /* i-digit of decimal number N may be obtained
         * by the following computation:
         *
         *                 i       i-1
         *    d  = ( N % 10  ) / 10
         *     i
         *
         * where i=1 gives the rightmost (i.e. first) digit.
         */

        left_10_i = tens;
        left_10_i_1 = left_10_i / 10;

        right_10_i = 10;
        right_10_i_1 = 1;

        for (i = 1; i <= digits / 2; i++) {
                left = (n % left_10_i) / left_10_i_1;
                right = (n % right_10_i) / right_10_i_1;

                if (left != right)
                        return 0;

                /* shift 'left*' to the right */
                left_10_i /= 10;
                left_10_i_1 /= 10;

                /* shift 'right*' to the left */
                right_10_i *= 10;
                right_10_i_1 *= 10;
        }
        return 1;
}


int main(int argc, char *argv[])
{
        int i, j, N = 0, ii, jj;
        for (i = 100; i <= 999; i++) {
                for (j = 100; j <= 999; j++) {
                        int n = i*j;
                        if (is_palindromic(n) && n > N) {
                               N = n;
                               ii = i;
                               jj = j;
                        }
                }
        }
        printf("%d = %d * %d\n", N, ii, jj);
        return 0;
}


/* end of file */
