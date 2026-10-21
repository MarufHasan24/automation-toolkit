(defun c:MCL ( / ss i j e1 e2 p1 p2 p3 p4 mid1 mid2 d tol)
  (setq tol 0.1) ;; tolerance for parallel check

  (setq ss (ssget '((0 . "LINE"))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e1 (ssname ss i))
        (setq p1 (cdr (assoc 10 (entget e1))))
        (setq p2 (cdr (assoc 11 (entget e1))))

        (setq j (1+ i))
        (while (< j (sslength ss))
          (setq e2 (ssname ss j))
          (setq p3 (cdr (assoc 10 (entget e2))))
          (setq p4 (cdr (assoc 11 (entget e2))))

          ;; Check if lines are parallel
          (if (< (abs (- (angle p1 p2) (angle p3 p4))) tol)
            (progn
              ;; Midpoints
              (setq mid1 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p3))
              (setq mid2 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p2 p4))

              ;; Distance check (avoid far pairs)
              (setq d (distance p1 p3))

              (if (< d 100) ;; adjust max wall thickness here
                (command "LINE" mid1 mid2 "")
              )
            )
          )
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )
    )
    (princ "\nNo lines selected.")
  )
  (princ)
)