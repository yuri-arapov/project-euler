#!/usr/bin/awk -f

BEGIN {
        n = 0
        l = ""
}

{
        ll = $0
        nn = $1
        if (nn == n) {
                print(l)
                print(ll)
        }
        l = ll
        n = nn
}

# end of file
