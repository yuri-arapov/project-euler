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


(defn divisible? [n divider] (zero? (rem n divider)))


(defn leap-year? [year]
  (or (divisible? year 400)
      (and (not (divisible? year 100))
           (divisible? year 4))))


;;                        J  F  M  A  M  J  J  A  S  O  N  D
(def days-per-month [nil 31 28 31 30 31 30 31 31 30 31 30 31])


(defn last-day-of-month [year month]
  (if (and (= month 2) (leap-year? year))
    (inc (get days-per-month month))
    (get days-per-month month)))


(defn sunday? [day-of-week] (= day-of-week 7))


(defn next-day-of-week [day]
  (if (sunday? day) 1
    (inc day)))


(defn count-sundays [first-year last-year]
  (loop [res 0, year 1900, month 1, day 1, day-of-week 1]
    (cond (> year last-year)
            ;; done
            res

          (> month 12)
            ;; start next year
            (recur res (inc year) 1 1 day-of-week)

          (> day (last-day-of-month year month))
            ;; start next month
            (recur res year (inc month) 1 day-of-week)

          :else
            (recur
              (if (and (<= first-year year) (= 1 day) (sunday? day-of-week))
                (inc res)
                res)
              year
              month
              (inc day)
              (next-day-of-week day-of-week)))))


(defn p19 []
  (count-sundays 1901 2000))

;; end of file
