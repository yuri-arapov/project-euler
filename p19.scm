;; 19 March 2009
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=19
;;
;; Problem 19
;; 14 June 2002
;; 
;; You are given the following information, but you may prefer to do some
;; research for yourself.
;; 
;;     * 1 Jan 1900 was a Monday.
;;
;;     * Thirty days has September,
;;       April, June and November.
;;       All the rest have thirty-one,
;;       Saving February alone,
;;       Which has twenty-eight, rain or shine.
;;       And on leap years, twenty-nine.
;;
;;     * A leap year occurs on any year evenly divisible by 4, but not on a
;;       century unless it is divisible by 400.
;; 
;; How many Sundays fell on the first of the month during the twentieth century
;; (1 Jan 1901 to 31 Dec 2000)?
;;
;; Answer: 171


(define (divisible? dividend divider)
  (zero? (remainder dividend divider)))


;; Test if it's a leap year.
(define (leap-year? year)
  (or (divisible? year 400)
      (and (not (divisible? year 100))
           (divisible? year 4))))


;; Number of days per month.               J  F  M  A  M  J  J  A  S  O  N  D
(define days-per-month (list->array 1 '(0 31 28 31 30 31 30 31 31 30 31 30 31)))


;; Return true if it's a Sunday.
(define (sunday? day-of-week)
  (= day-of-week 7))


;; Return number of days of given month on given year.
(define (last-day-of-month year month)
  (if (and (= month 2) (leap-year? year))
    29                          ;; February contains 29 days on leap year
    (array-ref days-per-month month)))


(define (count-sundays 
          seed-year             ;; refers to 1st of Jan of seed-year
          seed-day-of-week      ;; day of week of Jan 1st of seed-year
          first-year            ;; inclusive
          last-year)            ;; inclusive

  (define (iter 
            year 
            month 
            day-of-month 
            day-of-week
            res)

  (cond ((> year last-year) 
         ;; all done
         res)

        ((> month 12) 
         ;; switch to next year
         (iter (+ year 1)
               1                ;; year starts with 1st month
               1                ;; month starts with 1st day
               day-of-week
               res))

        ((> day-of-month (last-day-of-month year month))
         ;; switch to next month
         (iter year
               (+ month 1)
               1                ;; month starts with 1st day
               day-of-week
               res))

        (else
          ;; ordinary day
          (iter year                            ;; same year
                month                           ;; same month
                (+ 1 day-of-month)              ;; next day of month

                (if (sunday? day-of-week)       ;; next day of week
                  1
                  (+ 1 day-of-week))

                (if (and (<= first-year year last-year) ;; count Sundays that
                         (= 1 day-of-month)             ;; fell on 1st day of month
                         (sunday? day-of-week))         ;; in given range of years
                  (+ 1 res)
                  res)))))

  (iter 
    seed-year           ;; year 
    1                   ;; 1st month of the year
    1                   ;; 1st day of the month
    seed-day-of-week
    0))                 ;; result


;; end of file
