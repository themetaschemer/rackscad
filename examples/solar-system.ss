#lang racket
(require rackscad)

(struct celestial (diameter distance color))

(define sun      (celestial 139198    0 "yellow"))
(define mercury  (celestial 4880 58000000 "orange"))
(define venus    (celestial 12100 108000000 "dimgrey"))
(define earth    (celestial 12800 150000000 "blue"))
(define mars     (celestial 6800 228000000 "red"))
(define jupiter  (celestial 142000 778000000 "indianred"))
(define saturn   (celestial 120000 1430000000 "gold"))
(define uranus   (celestial 51800 2870000000 "ghostwhite"))
(define neptune  (celestial 49500 4500000000 "blueviolet"))
(define pluto    (celestial 2300 5900000000 "cornsilk"))

(define (dist-scale x) (* x (/ 2500 5900000000)))
(define (dia-scale x)  (* x (/ 1000000 5900000000)))

(define (celestial-body x-m)
  (color #:name (celestial-color x-m)
         (translate ((dist-scale (celestial-distance x-m)) 0 0)
                    (sphere #:diameter (dia-scale (celestial-diameter x-m))))))
(define solar-system
  (map celestial-body (list sun mercury venus earth mars jupiter saturn uranus neptune pluto)))

(compile "/tmp/sol.scad"  solar-system)


