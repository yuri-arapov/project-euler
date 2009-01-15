/* 25 December 2007
 *
 * Yuri Arapov <yuridichesky@gmail.com>
 *
 * Project Euler
 *
 * Problem 2
 *
 * http://projecteuler.net/index.php?section=problems&id=3
 *
 *
 * The prime factors of 13195 are 5, 7, 13 and 29.
 *
 * What is the largest prime factor of the number 317584931803?
 *
 * Answer:
 *      3919
 *
 * Done.
 *
 * See also ecm.java, found here (VERY fast):
 *   http://www.alpertron.com.ar/ECM.HTM
 */

#include <stdio.h>
#include <math.h>

#define NUMBER double

NUMBER max_prime_factor(NUMBER n)
/* determine and return max prime factor of the argument n
 * recursively.
 */
{
        NUMBER r, c = 2.0;
        if (n == 1.0)
                return n;
        for (;;) {
                if (c > n / 2.0)
                        /* no reason to increase c any more */
                        break;
                r = n / c;
                if (floor(r) == ceil(r)) {
                        /* c is divider of the n */
                        NUMBER p1 = max_prime_factor(r);
                        NUMBER p2 = max_prime_factor(c);
                        return p1 > p2 ? p1 : p2;
                }
                c += 1.0;
        }
        return n; /* n is a prime itself */
}


NUMBER max_prime_factor2(NUMBER n)
/* determine and return max prime factor of the n */ 
{
        NUMBER r, divisor = 2.0;
        if (n == 1.0)
                return n;
        while (n > 1.0) {
                r = n / divisor;
                if (floor(r) == ceil(r))
                        n = r;
                else
                        divisor += 1.0;
        }
        return divisor;
}

int main(int argc, char *argv[])
{
        NUMBER maxprime, n = 317584931803.0;
/*        n = 123432123432122232333337779999999977.0;*/
        maxprime = max_prime_factor2(n);
        printf("%.0f\n", maxprime);
        return 0;
}
