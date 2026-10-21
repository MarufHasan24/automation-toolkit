(defun c:RECTNUM ( / ss i ent obj minpt maxpt
                     width height txtHeight
                     cen sortedList count)

  (prompt "\nSelect rectangles: ")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))

  (if ss
    (progn
      (setq i 0)
      (setq sortedList '())

      ;; Collect rectangle data
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        (vla-getboundingbox obj 'minpt 'maxpt)

        (setq minpt (vlax-safearray->list minpt))
        (setq maxpt (vlax-safearray->list maxpt))

        (setq sortedList (cons (list minpt maxpt) sortedList))
        (setq i (1+ i))
      )

      ;; Sort left → right
      (setq sortedList
        (vl-sort sortedList
          '(lambda (a b)
             (< (car (car a)) (car (car b))))))

      (setq count 1)

      (foreach item sortedList

        (setq minpt (car item))
        (setq maxpt (cadr item))

        (setq width  (- (car maxpt) (car minpt)))
        (setq height (- (cadr maxpt) (cadr minpt)))

        ;; Text height = (height/20)*2 = height/10
        (setq txtHeight (/ height 10.0))

        ;; Center point
        (setq cen
          (list (/ (+ (car minpt) (car maxpt)) 2.0)
                (/ (+ (cadr minpt) (cadr maxpt)) 2.0)))

        ;; Create centered text
        (command "_.TEXT" "_J" "MC" cen txtHeight "0" (itoa count))

        (setq count (1+ count))
      )

      (prompt "\nCentered numbering completed.")
    )
    (prompt "\nNo rectangles selected.")
  )

  (princ)
)
