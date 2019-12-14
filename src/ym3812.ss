(define (rld)
  (load "ym3812.ss"))

(define (upl)
  (avr16-compile-and-upload code))

(define (hibits val)
  (.>> val 8))

(define (lobits val)
  (.& val #xff))

(define (code-len code)
  (+ 1 (fold max -1 (code-addr-image code))))

;

(define-macro (save-regs regs . body)
  (let ((pushes (map
                  (lambda (reg)
                    (cons 'push. (list reg)))
                  regs))
        (pops (map
                (lambda (reg)
                  (cons 'pop. (list reg)))
                (reverse regs))))
    (append (list 'list) pushes body pops)))

(define (loop . body)
  (let ((label-name (gensym)))
    (if (null? body)
        (list (% label-name)
              (rjmp. (%r label-name)))
        (list (% label-name)
              body
              (rjmp. (%r label-name))))))

(define (fn name . body)
  (if (null? body)
      (list (% name)
            (ret.))
      (list (% name)
            body
            (ret.))))

;

(define (symbol->bit-setter sym)
  (case sym
    ((in low off) cbi.)
    ((out high on) sbi.)
    (else
     (error "invalid symbol to bitsetter:" sym))))

; (pin-mode '(b . 5) 'out)
(define (pin-mode portpin sym)
  (let* ((port (car portpin))
         (pin (cdr portpin))
         (reg (case port
                ((b) &ddrb)
                ((c) &ddrc)
                ((d) &ddrd)
                (else "invalid port letter:" port))))
    ((symbol->bit-setter sym) reg pin)))

; (set-pin '(b . 5) 'high)
(define (set-pin portpin sym)
  (let* ((port (car portpin))
         (pin (cdr portpin))
         (reg (case port
                ((b) &portb)
                ((c) &portc)
                ((d) &portd)
                (else "invalid port letter:" port))))
    ((symbol->bit-setter sym) reg pin)))

(define (io-init io-reg . names)
  (let* ((snames (map symbol->string names))
         (bit-locs (map
                     (lambda (name)
                       (avr-register-bit-name->bit io-reg name))
                     snames))
         (mask
           (fold
             (lambda (pos acc)
               (.i acc (.<< 1 pos)))
             0
             bit-locs)))
    (list (ldi. &r16 mask)
          (out. (^io io-reg) &r16))))

(define (io-imm io-reg val)
  (list (ldi. &r16 val)
        (out. (^io io-reg) &r16)))

(define (nop2)
  (rjmp. 0))

(define (nop8)
  (call. 'nop8))

; stack stuff

(define (>y reg)
  (st-y. reg))

(define (y> reg)
  (ldy+. reg))

(define (y@ reg)
  (ldy. reg))

(define (y-call name . args)
  (let ((make-push
          (lambda (val)
            (list
              (ldi. &r16 val)
              (>y &r16)))))
   (append
     (collect make-push args)
     (list (call. name)))))

;

(define marker (basic 'marker 0))

(define ram
  (mem 'ram
    (@ #x0100)
    (var marker 'start)

    (@ #x06ff)
    (var marker 'stack-base)

    (@ #x08ff)
    (var marker 'end)))

;

; 16 - 21 are clobberable
; dont count on them being the same after a call

(define code '())

(define (add-code! lst)
  (set! code (append code lst)))

(define p-sck  '(b . 5))
(define p-miso '(b . 4))
(define p-mosi '(b . 3))
(define p-ss   '(b . 2))

(define p-sr-latch '(b . 1))

(define p-ym-clk '(d . 5))
(define p-ym-reset '(d . 6))
(define p-ym-a0 '(d . 7))
(define p-ym-wr '(b . 0))

(define p-debug '(c . 5))

; base

(add-code!
  (list
    (@ #x0000)
    (% 'resets)
    (jmp. 'begin)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)
    (nop.) (nop.)

    (@ #x34)
    (% 'begin)
    (io-imm &sph (hibits (offset-of ram 'end)))
    (io-imm &spl (lobits (offset-of ram 'end)))

    (ldi. &Yh (hibits (offset-of ram 'stack-base)))
    (ldi. &Yl (lobits (offset-of ram 'stack-base)))

    (call. 'spi-init)
    (call. 'sr-init)
    (call. 'ym-init)

    (pin-mode p-debug 'out)

    (y-call 'ym-write #x00 #x20)
    (y-call 'ym-write #x0f #x40)
    (y-call 'ym-write #xf0 #x60)
    (y-call 'ym-write #xf7 #x80)

    (y-call 'ym-write #x01 #x23)
    (y-call 'ym-write #x00 #x43)
    (y-call 'ym-write #xf0 #x63)
    (y-call 'ym-write #xf7 #x83)

    (loop
      (set-pin p-debug 'high)
      (y-call 'ym-write #x40 #xa0)
      (y-call 'ym-write #x2f #xb0)
      (y-call 'delay-ms 40)

      (set-pin p-debug 'low)
      (y-call 'ym-write #x40 #xa0)
      (y-call 'ym-write #x0f #xb0)
      (y-call 'delay-ms 100)

      (set-pin p-debug 'high)
      (y-call 'ym-write #x40 #xa0)
      (y-call 'ym-write #x2e #xb0)
      (y-call 'delay-ms 40)

      (set-pin p-debug 'low)
      (y-call 'ym-write #x40 #xa0)
      (y-call 'ym-write #x0e #xb0)
      (y-call 'delay-ms 100))))

; spi

(add-code!
  (list
    (fn 'spi-init
      (pin-mode p-sck  'out)
      (pin-mode p-miso 'in)
      (pin-mode p-mosi 'out)
      (pin-mode p-ss   'out)

      (io-init &spcr0 'spe0 'mstr0)

      (in. &r16 (^io &spsr0))
      (sbr. &r16 (^b &spsr0 'spi2x0))
      (out. (^io &spsr0) &r16))))

; shift register

(add-code!
  (list
    (fn 'sr-init
      (pin-mode p-sr-latch 'out)
      (call. 'sr-reset))

    (fn 'sr-reset
      (set-pin p-ss 'high)
      (set-pin p-ss 'low)
      (set-pin p-sr-latch 'low)
      (set-pin p-sr-latch 'high)
      (set-pin p-ss 'high))

    ; ( data -- )
    (fn 'sr-shift-out
      (y> &r16)
      (out. (^io &spdr0) &r16)

      (loop
        (in. &r17 (^io &spsr0))
        (sbrs. &r17 (^b &spsr0 'spif0)))

      (set-pin p-sr-latch 'low)
      (set-pin p-sr-latch 'high))))

; ym3812

(add-code!
  (list
    (fn 'ym-init
      (pin-mode p-ym-clk 'out)

      ; waveform generation mode: fast pwm
      ; compare mode: non inverting
      ; clock source: no prescale
      (io-init &tccr0a 'com0b1 'wgm01 'wgm00)
      (io-init &tccr0b 'wgm02 'cs00)

      ; 50% duty cycle
      ; 16/6 MHz or 2.667
      (io-imm &ocr0a 5)
      (io-imm &ocr0b 2)

      (pin-mode p-ym-reset 'out)
      (pin-mode p-ym-a0 'out)
      (pin-mode p-ym-wr 'out)

      (set-pin p-ym-clk 'low)

      (call. 'ym-reset))

    (fn 'ym-reset
      ; hold for 80 cycles
      ; 30us at 2.667 Mhz
      (set-pin p-ym-reset 'low)
      (y-call 'delay-us 50)
      (set-pin p-ym-reset 'high))

    ; ( data addr -- )
    (fn 'ym-write
      (call. 'sr-shift-out)
      (set-pin p-ym-a0 'low)
      (set-pin p-ym-wr 'low)
      (nop2)
      (set-pin p-ym-wr 'high)

      (y-call 'delay-us 50)

      (call. 'sr-shift-out)
      (set-pin p-ym-a0 'high)
      (set-pin p-ym-wr 'low)
      (nop2)
      (set-pin p-ym-wr 'high)

      (y-call 'delay-us 50))))

; util

(add-code!
  (list
    ; ( us -- )
    (fn 'delay-us
      (y> &r16)
      (dec. &r16)

      (loop
        (nop8)
        (nop2)
        (nop2)
        (dec. &r16)
        (breq. 1)))

    ; ( ms -- )
    (fn 'delay-ms
      (save-regs (&r22 &r23)
        (y> &r22)
        (ldi. &r23 250)
        (loop
          (>y &r23)
          (call. 'delay-us)
          (>y &r23)
          (call. 'delay-us)
          (>y &r23)
          (call. 'delay-us)
          (>y &r23)
          (call. 'delay-us)
          (dec. &r22)
          (breq. 1))))

    (fn 'nop8)))
