;;;; Procedures for manipulating digits in integer numbers
;;; Emilio C. Lopes <eclig@gmx.net>, 2006-02-03.

(define *digits-default-base* 10)

(define (digits number . maybe-base)
  ;; digits : integer -> (listof integer)
  ;;
  ;; Return the digits of integer NUMBER in the given base as a list.
  ;;
  ;; Examples:
  ;;
  ;; (digits 123) => '(1 2 3)
  ;; (digits -42) => '(4 2)
  ;; (digits 0) => '(0)
  ;; (digits #xef 16) => '(14 15)
  (let ((base (if (null? maybe-base) *digits-default-base* (car maybe-base))))
    (let loop ((number number)
               (result '()))
      (if (zero? number)
          (if (null? result) (list 0) result)
          (loop (quotient number base)
                (cons (abs (remainder number base)) result))))))

(define (last-digit number . maybe-base)
  ;; last-digit : integer -> integer
  ;;
  ;; Return the last digit of the integer NUMBER in the given base.
  ;;
  ;; Examples:
  ;;
  ;; (last-digit 123) => 3
  ;; (last-digit -42) => 2
  ;; (last-digit #xef 16) => 15
  (let ((base (if (null? maybe-base) *digits-default-base* (car maybe-base))))
    (abs (remainder number base))))

(define (first-digit number . maybe-base)
  ;; first-digit : integer integer -> integer
  ;;
  ;; Return the first digit of the integer NUMBER in the given base.
  ;;
  ;; Examples:
  ;;
  ;; (first-digit 123) => 1
  ;; (first-digit -42) => 4
  ;; (first-digit #xef 16) => 14
  (let ((base (if (null? maybe-base) *digits-default-base* (car maybe-base))))
    (let loop ((number number)
               (last 0))
      (if (zero? number)
          (abs last)
          (loop (quotient number base)
                (remainder number base))))))

(define (reverse-digits number . maybe-base)
  ;; reverse-digits : integer integer -> integer
  ;;
  ;; Return the number formed by reversing the order of the
  ;; digits of the integer NUMBER in the given base.
  ;;
  ;; Examples:
  ;;
  ;; (reverse-digits 123) => 321
  ;; (reverse-digits -42) => -24
  ;; (reverse-digits #xef 16) => 254
  (let ((base (if (null? maybe-base) *digits-default-base* (car maybe-base))))
    (let loop ((number number)
               (reversed 0))
      (if (zero? number)
          reversed
          (loop (quotient number base)
                (+ (* reversed base)
                   (remainder number base)))))))

(define (fold-digits digits . maybe-base)
  ;; reverse-digits :  (listof integer) -> integer
  ;;
  ;; Return the number formed by the digits in DIGITS in the given
  ;; base.
  ;;
  ;; Examples:
  ;;
  ;; (fold-digits '()) => 0
  ;; (fold-digits '(1 2 3)) => 123
  ;; (fold-digits '(-4 2)) => -38
  ;; (fold-digits '(#xe #xf) 16) => 239
  (let ((base (if (null? maybe-base) *digits-default-base* (car maybe-base))))
    (let loop ((digits digits)
               (number 0))
      (if (null? digits)
          number
          (loop (cdr digits)
                (+ (* number base)
                   (car digits)))))))
