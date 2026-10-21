(defun c:Blk2Pline ( / ss plSel plObj i ent blkObj newObj inspt rot)

  (vl-load-com)

  ;; Select blocks
  (setq ss (ssget '((0 . "INSERT"))))
  (if (not ss)
    (progn (princ "\nNo blocks selected.") (exit))
  )

  ;; Select polyline
  (setq plSel (entsel "\nSelect polyline: "))
  (if (not plSel)
    (progn (princ "\nNo polyline selected.") (exit))
  )

  (setq plObj (vlax-ename->vla-object (car plSel)))

  (setq i 0)
  (while (< i (sslength ss))

    (setq ent (ssname ss i))
    (setq blkObj (vlax-ename->vla-object ent))

    ;; Get data
    (setq inspt (vlax-get blkObj 'InsertionPoint))
    (setq rot   (vlax-get blkObj 'Rotation))

    ;; Copy polyline
    (setq newObj (vla-copy plObj))

    ;; 🔥 FIX: proper 3D points
    (vla-move newObj
      (vlax-3d-point '(0 0 0))
      (vlax-3d-point inspt)
    )

    ;; Rotate
    (vla-rotate newObj
      (vlax-3d-point inspt)
      rot
    )

    ;; Delete block
    (vla-delete blkObj)

    (setq i (1+ i))
  )

  (princ "\n✅ Done.")
  (princ)
)