/* 23 December, 2007
 *
 * Project Euler
 *
 * user: yuridichesky
 *
 * Problem 1.
 *
 * http://projecteuler.net/index.php?section=problems&id=1
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 *
 * If we list all the natural numbers below 10 that are
 * multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these
 * multiples is 23.
 *
 * Answer:
 *      233168
 *
 * Done.
 */

#include <stdio.h>

int sum_of_multiples(int multiplier,
                     int upper_bound /* excluded */)
/* FIXME: */
{
        int N = (upper_bound - 1) / multiples;
        int halfN = N / 2;
        int sum = (multiplier + multiplier*N) * halfN;
        if ((N % 2) == 1)
                sum += multiplier * (halfN + 1);
        return sum;
}


int main(int argc, char *argv[])
{
        printf("%d\n", sum_of_multipless(3, 1000) + 
                       sum_of_multipless(5, 1000) - 
                       sum_of_multipless(3*5, 1000));
        return 0;
}
