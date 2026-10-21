(defun c:MOFST (/ ss dist side i ent obj)
  (vl-load-com)

  ;; Select multiple polylines
  (prompt "\nSelect polylines to offset: ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))

  (if ss
    (progn

      ;; Ask for offset distance
      (setq dist (getdist "\nEnter offset distance: "))

      ;; Ask for direction
      (initget "Inside Outside")
      (setq side (getkword "\nOffset direction [Inside/Outside]: "))

      (if (and dist side)
        (progn
          (setq i 0)

          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq obj (vlax-ename->vla-object ent))

            ;; Determine offset direction
            (if (= side "Inside")
              (setq offsetDist (- dist))
              (setq offsetDist dist)
            )

            ;; Offset
            (vla-Offset obj offsetDist)

            (setq i (1+ i))
          )

          (princ "\nOffset completed.")
        )
      )
    )
    (prompt "\nNo polylines selected.")
  )

  (princ)
)