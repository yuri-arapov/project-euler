/* Project Euler
 *
 * http://projecteuler.net/index.php?section=problems&id=10
 *
 * Problem 10
 * 08 February 2002
 *
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 * Answer:
 *      37550402023 (was: one million)
 *      142913828922
 *
 * FIXME: worth to try: http://en.wikipedia.org/wiki/Sieve_of_Atkin
 */


#include <stdlib.h>
#include <stdio.h>
#include <math.h>


#define BYTES_PER_INT 4
#define BITS_PER_INT  32
#define ALL_ON        0xffffffff


static const unsigned int Nth_bit[] = {
        0x01 <<  0,
        0x01 <<  1,
        0x01 <<  2,
        0x01 <<  3,
        0x01 <<  4,
        0x01 <<  5,
        0x01 <<  6,
        0x01 <<  7,
        0x01 <<  8,
        0x01 <<  9,
        0x01 << 10,
        0x01 << 11,
        0x01 << 12,
        0x01 << 13,
        0x01 << 14,
        0x01 << 15,
        0x01 << 16,
        0x01 << 17,
        0x01 << 18,
        0x01 << 19,
        0x01 << 20,
        0x01 << 21,
        0x01 << 22,
        0x01 << 23,
        0x01 << 24,
        0x01 << 25,
        0x01 << 26,
        0x01 << 27,
        0x01 << 28,
        0x01 << 29,
        0x01 << 30,
        0x01 << 31
};


#define NTH_BIT(bit)             Nth_bit[bit]
/* return an integer containing only Nth bit turend on 
 * (where N is in 0..31 range)
 */


#define BIT_TO_INDEX(bit)        ( (bit) / BITS_PER_INT )
/* return index of array member containing given bit */


#define BIT_TO_BIT(bit)          ( (bit) % BITS_PER_INT )
/* return number of the bit (in 0..31 range) inside of
 * array member corresponding given bit
 */


#define SET_BIT(array, bit)      array[ BIT_TO_INDEX(bit) ] |= NTH_BIT( BIT_TO_BIT(bit) )
/* turn on given bit in bitarray */


#define UNSET_BIT(array, bit)    array[ BIT_TO_INDEX(bit) ] &= ~NTH_BIT( BIT_TO_BIT(bit) )
/* turn off given bit in bitarray */


#define IS_BIT_SET(array, bit)   ( array[ BIT_TO_INDEX(bit) ] & NTH_BIT( BIT_TO_BIT(bit) ) )
/* return TRUE if given bit is turned on in bitarray */


#define PRIMES_UPPER_BOUND 2000000
/* max value to test for the primes */


int main(void)
{
        unsigned int *bitarray = NULL;
        unsigned int bitarray_size;
        unsigned int i, j, n;

        double s;

        if (sizeof(unsigned int) != BYTES_PER_INT) {
                fprintf(stderr, "size of 'unsigned int' is expected to be %d instead of %d\n", 
                        BYTES_PER_INT, 
                        sizeof(unsigned int));
                return 1;
        }

        /* this is how many 'unsigned int's needed to store PRIMES_UPPER_BOUND bits */
        bitarray_size = PRIMES_UPPER_BOUND / BITS_PER_INT 
                      + ((PRIMES_UPPER_BOUND % BITS_PER_INT) == 0 ? 0 : 1);

/*        printf("PRIMES_UPPER_BOUND=%d\n", PRIMES_UPPER_BOUND); */
/*        printf("BITS_PER_INT=%d\n", BITS_PER_INT); */
/*        printf("bitarray_size=%d\n", bitarray_size); */

        /* allocate bitarray and turn all the bits on */
        bitarray = (unsigned int*)malloc(sizeof(unsigned int)*bitarray_size);
        for (i = 0; i < bitarray_size; i++)
                bitarray[i] = ALL_ON;

        /* determine prime numbers by Sieve of Eratosthenes */
        for (i = 2; i <= PRIMES_UPPER_BOUND; i++) {
                if (! IS_BIT_SET(bitarray, i))
                        /* i is not a prime */
                        continue;
                /* i is prime: remove from the list all the numbers 
                 * divisible by i except itself
                 */
/*                printf("prime %6d\n", i); */
                for (n = 2; ; n++) {
                        j = i*n;
                        if (j >= PRIMES_UPPER_BOUND)
                                break;
                        UNSET_BIT(bitarray, j);
                }
        }

        /* compute sum of all the primes */
        s = 0;
        for (i = 2; i < PRIMES_UPPER_BOUND; i++)
                if (IS_BIT_SET(bitarray, i))
                        s += i;

        printf("%.0f\n", s);

        free(bitarray);
        return 0;
}

/* end of file */

