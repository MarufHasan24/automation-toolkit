(defun c:DimS ( / ss i ent obj pts j p1 p2 mid ang offset dist)

  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))

  (if ss
    (progn
      (setq i 0)

      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        ;; ---------- LINE ----------
        (if (= (vla-get-ObjectName obj) "AcDbLine")
          (progn
            (setq p1 (vlax-get obj 'StartPoint))
            (setq p2 (vlax-get obj 'EndPoint))

            (command "_DIMALIGNED" p1 p2 (polar p1 (/ pi 2) 5.0))
          )
        )

        ;; ---------- POLYLINE ----------
        (if (= (vla-get-ObjectName obj) "AcDbPolyline")
          (progn
            (setq coords (vlax-get obj 'Coordinates))
            (setq pts '())

            ;; convert to point list
            (while coords
              (setq pts (append pts (list (list (car coords) (cadr coords)))))
              (setq coords (cddr coords))
            )

            ;; loop segments
            (setq j 0)
            (while (< j (1- (length pts)))
              (setq p1 (nth j pts))
              (setq p2 (nth (1+ j) pts))

              ;; midpoint
              (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))

              ;; angle perpendicular offset
              (setq ang (+ (angle p1 p2) (/ pi 2)))
              (setq offset (polar mid ang 5.0)) ;; change 5.0 to adjust spacing

              (command "_DIMALIGNED" p1 p2 offset)

              (setq j (1+ j))
            )

            ;; closed polyline last segment
            (if (= (vla-get-Closed obj) :vlax-true)
              (progn
                (setq p1 (last pts))
                (setq p2 (car pts))

                (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2))
                (setq ang (+ (angle p1 p2) (/ pi 2)))
                (setq offset (polar mid ang 5.0))

                (command "_DIMALIGNED" p1 p2 offset)
              )
            )
          )
        )

        (setq i (1+ i))
      )
    )
    (prompt "\nNo valid objects selected.")
  )

  (princ)
)