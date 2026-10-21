;; ============================================================
;; SCATTER  -  Random multi-tree block scatter for AutoCAD
;;
;; - Boundary: LWPOLYLINE, POLYLINE (2D/3D), CIRCLE, ELLIPSE, SPLINE
;;   (arc/bulge segments are handled by flattening the curve)
;; - Select any number of tree blocks; one is picked at random each time
;; - Random position, rotation and scale
;; - Optional minimum spacing between trees
;; - Trees are COPIES of the selected blocks, so layers, attributes and
;;   dynamic block settings are preserved
;; - Whole run is one UNDO step
;; - 32-bit safe random generator (no integer overflow)
;; ============================================================

(vl-load-com)

;; ============================================================
;; RANDOM NUMBER SYSTEM  (Park-Miller, computed with Schrage's
;; method so no intermediate value exceeds 32 bits)
;; ============================================================

(setq *ST-SEED* 1)

(defun ST:Rand01 ( / hi lo test )
  ;; m = 2147483647, a = 16807, q = 127773, r = 2836
  (setq hi   (/ *ST-SEED* 127773)
        lo   (rem *ST-SEED* 127773)
        test (- (* 16807 lo) (* 2836 hi)))
  (setq *ST-SEED* (if (> test 0) test (+ test 2147483647)))
  (/ (float *ST-SEED*) 2147483647.0)
)

(defun ST:SetSeed ( / s )
  (setq s (rem (abs (+ (getvar "MILLISECS") 12345)) 2147483647))
  (if (< s 1) (setq s 1))
  (setq *ST-SEED* s)
  ;; warm up so the first values are not correlated with the seed
  (repeat 8 (ST:Rand01))
  *ST-SEED*
)

;; Number between lo and hi
(defun ST:RandBetween (lo hi)
  (+ lo (* (ST:Rand01) (- hi lo)))
)

;; Integer 0 .. n-1
(defun ST:RandIndex (n / k)
  (if (> n 0)
    (progn
      (setq k (fix (* (ST:Rand01) n)))
      (if (>= k n) (setq k (1- n)))
      (if (< k 0) (setq k 0))
      k
    )
    0
  )
)

;; ============================================================
;; FLATTEN A BOUNDARY CURVE INTO A LIST OF (X Y Z) POINTS
;; Works in WCS, so OCS polylines and arcs (bulges) are correct.
;; ============================================================

(defun ST:FlattenCurve (ent / typ sp ep closed pts i p0 p1 pm mid k steps len n)
  (setq typ (cdr (assoc 0 (entget ent))))
  (cond
    ;; ---- polylines: walk segment by segment, subdivide curved ones
    ((member typ '("LWPOLYLINE" "POLYLINE"))
     (setq sp     (vlax-curve-getStartParam ent)
           ep     (vlax-curve-getEndParam ent)
           closed (vlax-curve-isClosed ent)
           steps  32
           i      sp
           pts    nil)
     (while (< i (- ep 0.5))
       (setq p0 (vlax-curve-getPointAtParam ent i)
             p1 (vlax-curve-getPointAtParam ent (+ i 1.0))
             pm (vlax-curve-getPointAtParam ent (+ i 0.5))
             mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p0 p1))
       (setq pts (cons p0 pts))
       ;; midpoint off the chord => curved segment
       (if (> (distance pm mid) (+ 1e-9 (* 1e-6 (distance p0 p1))))
         (progn
           (setq k 1)
           (while (< k steps)
             (setq pts (cons (vlax-curve-getPointAtParam ent (+ i (/ k (float steps)))) pts))
             (setq k (1+ k))
           )
         )
       )
       (setq i (+ i 1.0))
     )
     (if (not closed)
       (setq pts (cons (vlax-curve-getPointAtParam ent ep) pts))
     )
     (reverse pts)
    )
    ;; ---- other curves: uniform sampling by distance
    ((member typ '("CIRCLE" "ELLIPSE" "SPLINE"))
     (setq len (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent))
           n   256
           k   0
           pts nil)
     (while (< k n)
       (setq pts (cons (vlax-curve-getPointAtDist ent (* len (/ k (float n)))) pts))
       (setq k (1+ k))
     )
     (reverse pts)
    )
    (T nil)
  )
)

;; ============================================================
;; POINT IN POLYGON (ray casting, XY only)
;; ============================================================

(defun ST:PointInPoly (px py poly / inside prev xi yi xj yj)
  (setq inside nil
        prev   (last poly))
  (foreach cur poly
    (setq xi (car cur)  yi (cadr cur)
          xj (car prev) yj (cadr prev))
    (if (and
          (or (and (<= yi py) (< py yj))
              (and (<= yj py) (< py yi)))
          ;; division is safe: the test above guarantees yi <> yj
          (< px (+ xi (* (/ (- py yi) (- yj yi)) (- xj xi)))))
      (setq inside (not inside))
    )
    (setq prev cur)
  )
  inside
)

;; ============================================================
;; MINIMUM DISTANCE CHECK (squared distance, stops at first hit)
;; ============================================================

(defun ST:FarEnough (x y pts d2 / ok p dx dy)
  (setq ok T)
  (while (and ok pts)
    (setq p  (car pts)
          dx (- x (car p))
          dy (- y (cadr p)))
    (if (< (+ (* dx dx) (* dy dy)) d2)
      (setq ok nil)
    )
    (setq pts (cdr pts))
  )
  ok
)

;; ============================================================
;; PROMPT HELPERS
;; ============================================================

(defun ST:AskInt (msg def / v)
  (initget 6) ; no zero, no negative
  (setq v (getint (strcat "\n" msg " <" (itoa def) ">: ")))
  (if v v def)
)

(defun ST:AskReal (msg def flags / v)
  (initget flags)
  (setq v (getreal (strcat "\n" msg " <" (rtos def 2 2) ">: ")))
  (if v v def)
)

;; ============================================================
;; MAIN COMMAND
;; ============================================================

(defun c:SCATTER
  ( / *error* doc undoOpen
      sel bEnt poly
      ss i trees nTrees
      count minDist d2
      minScale maxScale minRot maxRot tmp
      minX minY maxX maxY z0
      x y points placed attempts maxAttempts failed abort
      src newObj sc rot
    )

  ;; ----------------------------------------------------------
  ;; Error handler: close the undo group, report real errors
  ;; ----------------------------------------------------------
  (defun *error* (msg)
    (if (and doc undoOpen)
      (progn (vla-EndUndoMark doc) (setq undoOpen nil))
    )
    (if (and msg
             (not (member (strcase msg) '("FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (ST:SetSeed)

  (princ "\n==========================================")
  (princ "\n           RANDOM TREE SCATTER")
  (princ "\n==========================================")

  ;; ----------------------------------------------------------
  ;; Boundary (re-prompts until valid or Enter)
  ;; ----------------------------------------------------------
  (setq poly nil)
  (while (and (null poly)
              (setq sel (entsel "\nSelect boundary (polyline, circle, ellipse or spline): ")))
    (setq bEnt (car sel))
    (setq poly
      (vl-catch-all-apply 'ST:FlattenCurve (list bEnt))
    )
    (if (or (vl-catch-all-error-p poly)
            (null poly)
            (< (length poly) 3))
      (progn
        (princ "\nThat object cannot be used as a boundary. Try again or press Enter to cancel.")
        (setq poly nil)
      )
    )
  )

  (cond
    ((null poly)
     (princ "\nNo boundary selected.")
    )

    (T
     (if (not (vl-catch-all-apply 'vlax-curve-isClosed (list bEnt)))
       (princ "\nNote: boundary is not closed; the last point will be joined to the first.")
     )

     ;; --------------------------------------------------------
     ;; Tree blocks
     ;; --------------------------------------------------------
     (princ "\nSelect the tree BLOCKS.")
     (setq ss (ssget '((0 . "INSERT"))))

     (cond
       ((null ss)
        (princ "\nNo tree blocks selected.")
       )

       (T
        (setq trees nil i 0)
        (repeat (sslength ss)
          (setq trees (cons (vlax-ename->vla-object (ssname ss i)) trees))
          (setq i (1+ i))
        )
        (setq trees  (reverse trees)
              nTrees (length trees))
        (princ (strcat "\nTree blocks selected: " (itoa nTrees)))

        ;; ------------------------------------------------------
        ;; Options
        ;; ------------------------------------------------------
        (setq count    (ST:AskInt  "Number of trees" 100))
        (setq minDist  (ST:AskReal "Minimum spacing" 0.0 4))   ; >= 0
        (setq minScale (ST:AskReal "Minimum scale" 0.8 6))     ; > 0
        (setq maxScale (ST:AskReal "Maximum scale" 1.2 6))     ; > 0
        (setq minRot   (ST:AskReal "Minimum rotation in degrees" 0.0 0))
        (setq maxRot   (ST:AskReal "Maximum rotation in degrees" 360.0 0))

        ;; Swap reversed ranges instead of producing odd results
        (if (> minScale maxScale)
          (setq tmp minScale minScale maxScale maxScale tmp))
        (if (> minRot maxRot)
          (setq tmp minRot minRot maxRot maxRot tmp))

        (setq minRot (* pi (/ minRot 180.0))
              maxRot (* pi (/ maxRot 180.0))
              d2     (* minDist minDist))

        ;; ------------------------------------------------------
        ;; Bounding box + insertion elevation from the boundary
        ;; ------------------------------------------------------
        (setq minX (apply 'min (mapcar 'car  poly))
              maxX (apply 'max (mapcar 'car  poly))
              minY (apply 'min (mapcar 'cadr poly))
              maxY (apply 'max (mapcar 'cadr poly))
              z0   (caddr (car poly)))

        ;; Real arithmetic avoids 32-bit overflow for large counts
        (setq maxAttempts (fix (min (* (float count) 2000.0) 2000000000.0)))

        (setq points nil placed 0 attempts 0 failed 0 abort nil)

        (princ "\nScattering trees...")
        (vla-StartUndoMark doc)
        (setq undoOpen T)

        ;; ------------------------------------------------------
        ;; Scatter loop
        ;; ------------------------------------------------------
        (while (and (< placed count) (< attempts maxAttempts) (not abort))
          (setq attempts (1+ attempts))
          (setq x (ST:RandBetween minX maxX)
                y (ST:RandBetween minY maxY))

          (if (and (ST:PointInPoly x y poly)
                   (or (<= minDist 0.0)
                       (ST:FarEnough x y points d2)))
            (progn
              (setq src (nth (ST:RandIndex nTrees) trees)
                    sc  (ST:RandBetween minScale maxScale)
                    rot (ST:RandBetween minRot maxRot))

              (setq newObj (vl-catch-all-apply 'vla-Copy (list src)))

              (if (vl-catch-all-error-p newObj)
                ;; Copy failed (e.g. locked layer): give up after 5 failures
                (progn
                  (setq failed (1+ failed))
                  (if (>= failed 5)
                    (progn
                      (setq abort T)
                      (princ "\nStopped: the tree blocks cannot be copied (locked layer?).")
                    )
                  )
                )
                (progn
                  ;; Dynamic blocks with locked scale may refuse; ignore that
                  (vl-catch-all-apply
                    '(lambda ()
                       (vla-put-XScaleFactor newObj sc)
                       (vla-put-YScaleFactor newObj sc)
                       (vla-put-ZScaleFactor newObj sc)))
                  (vla-put-Rotation newObj rot)
                  (vla-put-InsertionPoint newObj (vlax-3d-point x y z0))

                  (setq points (cons (list x y) points))
                  (setq placed (1+ placed))

                  (if (= (rem placed 25) 0)
                    (princ (strcat "\nPlaced " (itoa placed) " / " (itoa count)))
                  )
                )
              )
            )
          )
        )

        (vla-EndUndoMark doc)
        (setq undoOpen nil)

        ;; ------------------------------------------------------
        ;; Summary
        ;; ------------------------------------------------------
        (princ "\n\n==========================================")
        (princ "\nFinished!")
        (princ (strcat "\nTrees placed: " (itoa placed) " / " (itoa count)))
        (princ "\n==========================================")
        (if (< placed count)
          (princ "\nCould not place all trees. Try a smaller spacing or fewer trees.")
        )
       )
     )
    )
  )

  (princ)
)

(princ "\nSCATTER loaded. Type SCATTER to run.")
(princ)
