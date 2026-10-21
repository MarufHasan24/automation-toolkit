```lisp
;;; ============================================================
;;; RANDOM BLOCK PLACER
;;; Select a block -> choose scale range -> click to place
;;; Each copy gets a random scale and random rotation.
;;; ============================================================

(defun c:RBP (/ ent obj minScale maxScale p1 p2 scale rot
                basePt insPt newObj)

  (vl-load-com)

  ;; ------------------------------------------------------------
  ;; Select block
  ;; ------------------------------------------------------------
  (setq ent (car (entsel "\nSelect the block: ")))

  (if (and ent
           (= (cdr (assoc 0 (entget ent))) "INSERT"))
    (progn

      ;; --------------------------------------------------------
      ;; Ask for scale range
      ;; --------------------------------------------------------
      (setq minScale
        (getreal "\nEnter MINIMUM scale <0.5>: ")
      )

      (if (null minScale)
        (setq minScale 0.5)
      )

      (setq maxScale
        (getreal "\nEnter MAXIMUM scale <2.0>: ")
      )

      (if (null maxScale)
        (setq maxScale 2.0)
      )

      ;; Make sure min <= max
      (if (> minScale maxScale)
        (progn
          (setq p1 minScale)
          (setq minScale maxScale)
          (setq maxScale p1)
        )
      )

      ;; --------------------------------------------------------
      ;; Get insertion point of selected block
      ;; --------------------------------------------------------
      (setq basePt (cdr (assoc 10 (entget ent))))

      ;; --------------------------------------------------------
      ;; Start placement loop
      ;; --------------------------------------------------------
      (prompt
        "\nClick locations to place random copies."
      )
      (prompt
        "\nPress ENTER or ESC to finish."
      )

      (while
        (setq insPt
          (getpoint "\nSpecify insertion point: ")
        )


	;; Random scale between minScale and maxScale
	(setq scale
  		(+ minScale
     			(* (rand) (- maxScale minScale)) 
  			)
		)

	(princ scale)
	
        ;; Random rotation: 0 - 360 degrees
        (setq rot
          (* 2.0 pi (rand))
        )

        ;; Copy selected block
        (setq newObj
          (vla-Copy
            (vlax-ename->vla-object ent)
          )
        )

        ;; Move copied block from original insertion point
        ;; to clicked point
        (vla-Move
          newObj
          (vlax-3D-point basePt)
          (vlax-3D-point insPt)
        )

        ;; Apply random scale
        (vla-put-XScaleFactor newObj scale)
        (vla-put-YScaleFactor newObj scale)
        (vla-put-ZScaleFactor newObj scale)

        ;; Apply random rotation
        (vla-put-Rotation newObj rot)
      )

      (princ "\nRandom block placement finished.")
    )

    (prompt "\nThe selected object is not a block.")
  )

  (princ)
)

;;; ============================================================
;;; Random number initialization
;;; ============================================================

(defun rand (/)
  ;; Returns a random number between 0.0 and 1.0
  (setq *rbp-seed*
    (rem
      (+ (* 1103515245
            (if *rbp-seed* *rbp-seed* 12345)
         )
         12345
      )
      2147483647
    )
  )

  ;; Force result to positive 0.0 - 1.0
  (/ (float (abs *rbp-seed*)) 2147483647.0)
)

(princ "\nRandom Block Placer loaded. Type RBP to start.")
(princ)
```