(defun c:Z2B (/ ss i ent obj area minpt maxpt cen rad)
  (vl-load-com)

  (if (setq ss (ssget '((0 . "LWPOLYLINE"))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))

        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        (if (= (vla-get-closed obj) :vlax-true)
          (progn
            (setq area (vla-get-area obj))

            (vla-getboundingbox obj 'minpt 'maxpt)

            	(setq minpt (vlax-safearray->list minpt))
		(setq maxpt (vlax-safearray->list maxpt))

            (setq cen
              (list
                (/ (+ (car minpt) (car maxpt)) 2.0)
                (/ (+ (cadr minpt) (cadr maxpt)) 2.0)
                0.0
              )
            )

            (setq rad (sqrt (/ area pi)))

            (command "_.circle" cen rad)
          )
        )

        (setq i (1+ i))
      )
    )
  )
  (princ)
)