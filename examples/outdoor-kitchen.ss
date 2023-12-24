#lang racket

(require rackscad)


;;********************************************************************************
;; Appliances
;; For now they are modeled as boxes and cutouts.

(define (griddle-module)
  (cut-from (structural-module-4ft 48)
            (griddle)
            #:at '(center front top)))
(define (burner-module)
  (cut-from (cut-from (structural-module-4ft 48)
                       (power-burner)
                       #:at '(center-left front top))
             (tandoor)
             #:at '(center-right center top)))

(define (sink-module)
  (cut-from (structural-module-4ft 48)
            (sink)
            #:at '(center-right center top)))

(define (fridge-module)
  (cut-from (structural-module-4ft 48)
            (refrigerator)
            #:at '(center-left front bottom)))

(define (back-splash-and-bar)
  (stack-top (back-splash-4ft-rise-6-in) (bar-4ft-depth-12-in)
             #:align '(right front)))

(define (long-l)
  (stack-right
   (turn #:clockwise (degrees 90) #:axis 'z
         (back-splash-and-bar))
   (stack-front (stack-right (back-splash-and-bar) (back-splash-and-bar) (back-splash-and-bar) #:align '(front bottom))
                (stack-right sink-module griddle-module burner-module #:align ('front bottom))
                #:align '(right bottom))
   #:align '(front bottom)))

(define (short-l)
  (turn #:clockwise (degrees 90) #:axis 'z
        (stack-front (back-splash-and-bar) (fridge-module) #:align '(left bottom))))

(define (kitchen-structure)
  (stack-front (long-l) (short-l)
               #:align '(left bottom)))

;;********************************************************************************
;; Appliances
;; For now they are modeled as boxes and cutouts. 

(define-solid (griddle)
  (model "BLZ-GRIDDLE-LTE-NG")
  (manufacturer "Blaze")
  (url "https://www.bbqguys.com/blaze-outdoor-products/lte-30-inch-built-in-propane-gas-griddle-with-lights-blz-griddle-lte-lp")
  (box 30 28.37 12.37)
  (cutout 28.125 24.375 12)
  (color "silver"))

(define-solid (power-burner)
  (model "BLZ-PROPB-NG")
  (manufacturer "Blaze")
  (url "https://www.bbqguys.com/blaze-outdoor-products/professional-built-in-natural-gas-high-performance-power-burner-w-wok-ring-stainless-steel-lid-blz-propb-ng")
  (box 15.75 23.62 10.25)
  (cutout 14 19.5 9.25)
  (color "silver"))

(define-solid (tandoor)
  (model "Homdoor Outdoor Kitchen Custom Installation Natural Gas Tandoor")
  (manufacturer "Homdoor")
  (url "https://www.homdoor.com/outdoor-kitchen-custom-installation-natural-gas-tandoor-oven.html")
  (box 30 30 24)
  (cutout 30 30 24))

(define-solid (sink)
  (model "B-PS21")
  (manufacturer "Sunstone Products")
  (url "https://www.homedepot.com/p/Sunstone-Premium-Drop-In-Sink-with-Hot-and-Cold-Water-Faucet-and-Cutting-Board-B-PS21/301975831")
  (box 20.75 21 12)
  (cutout 18.25 18.5 9.875)
  (color "silver"))

(define-solid (refrigerator)
  (model "BLZ-SSRF-50DH")
  (manufacturer "Blaze")
  (url "https://www.bbqguys.com/blaze-outdoor-products/24-inch-5-2-cu-ft-outdoor-stainless-steel-compact-refrigerator-ul-approved-blz-ssrf-5-0dh")
  (box 23.625 23.687 33.5)
  (cutout 24.5 24 34)
  (color "silver"))

;;********************************************************************************
;; Structural Elements
;; These are just boxes for now -- we're not modeling the detailed framing
;; 

;; 4' length is actually 51.25. For now we're assuming 48" since it can be cut. 
(define-solid (structural-module-4ft length)
  (model "4-ft module")
  (manufacturer "BBQ Coach")
  (url "https://www.bbqcoach.com/4-ft-module-with-welded-tube-corners-40-off-this-sale-only/")
  (box length 32 35.25)
  (color "slategray"))

(define-solid (back-splash-4ft-rise-6-in)
  (model "4-ft backsplash")
  (manufacturer "BBQ Coach")
  (url "https://www.bbqcoach.com/4-ft-back-splash-kit-6-high-cut-2-fit-value-price/")
  (box 48 2.5 41)
  (color "slategray"))

(define-solid (bar-4ft-depth-12-in)
  (model "12-in Overhang Kit")
  (manufacturer "BBQ Coach")
  (url "https://www.bbqcoach.com/4-ft-of-12-over-hang-kit-cut-2-fit-value-price/")
  (box 47.875  2.5 12)
  (color "slategray"))

(define-solid (foot-rest-4ft-6in-x-6in)
  (model "6-in x 6-in footrest Kit")
  (manufacturer "BBQ Coach")
  (url "https://www.bbqcoach.com/4-ft-of-6-x-6-foot-rest-kit/")
  (box 47.875  2.5 12)
  (color "slategray"))

(define-solid (cement-board width depth face)
  (model "FLB60L")
  (manufacturer "Wonderboard")
  (url "https://www.homedepot.com/p/Custom-Building-Products-WonderBoard-Lite-5-ft-x-3-ft-x-1-4-in-Backer-Board-FLB60L/203689288")
  (box (match face ('top width) ('front width) ('side 0.25))
       (match face ('top depth) ('front 0.25)  ('side width))
       (match face ('top  0.25) ('front depth) ('side depth)))
  (color "lightgray"))

(define-solid (stone-facade width depth face)
  (model "DBROWAV624-3DH")
  (manufacturer "Brown Wave")
  (url "https://www.homedepot.com/p/MSI-Brown-Wave-3D-Ledger-Panel-6-in-x-24-in-Honed-Sandstone-Wall-Tile-10-cases-60-sq-ft-pallet-DBROWAV624-3DH/205960158")
  (box (match face ('top width) ('front width) ('side 1.5))
       (match face ('top depth) ('front 1.5)  ('side width))
       (match face ('top  1.5) ('front depth) ('side depth)))
  (color "brown"))

(define-solid (counter-top width depth face)
  (model "MSI Black granite tile")
  (manufacturer "MSI")
  (url "https://www.homedepot.com/p/MSI-Absolute-Black-12-in-x-12-in-Polished-Granite-Floor-and-Wall-Tile-10-sq-ft-case-TABSBLK1212/202508270")
  (box (match face ('top width) ('front width) ('side 0.75))
       (match face ('top depth) ('front 0.75)  ('side width))
       (match face ('top  0.75) ('front depth) ('side depth)))
  (color "black"))




