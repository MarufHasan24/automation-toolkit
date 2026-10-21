(defun c:RECTPLOTA3 ( / ss i ent obj minpt maxpt
                        ll ur baseName fileName folder numStr)

  (setq folder (getvar "DWGPREFIX"))
  (setq baseName (getstring "\nEnter base PDF name: "))

  (prompt "\nSelect rectangles to print: ")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))

  (if ss
    (progn
      (setq i 0)

      (while (< i (sslength ss))

        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        (vla-getboundingbox obj 'minpt 'maxpt)

        (setq ll (vlax-safearray->list minpt))
        (setq ur (vlax-safearray->list maxpt))

        ;; Create 01, 02, 03 format
        (setq numStr
          (if (< (1+ i) 10)
            (strcat "0" (itoa (1+ i)))
            (itoa (1+ i))
          )
        )

        (setq fileName
          (strcat folder baseName "_" numStr ".pdf"))

        (command
          "-plot"
          "Y"                                  ; Detailed plot
          ""                                   ; Current layout
          "DWG To PDF.pc3"
          "ISO A3 (420.00 x 297.00 MM)"
          "Millimeters"
          "Landscape"
          "No"                                 ; Plot upside down?
          "Window"
          ll ur
          "Fit"
          "Center"
          "Yes"                                ; Plot with plot styles?
          "monochrome.ctb"                     ; Plot style (IMPORTANT)
          "Yes"                                ; Plot lineweights?
          "No"                                 ; Scale lineweights?
          "No"                                 ; Plot paper space first?
          "No"                                 ; Hide paperspace?
          fileName
          "No"                                 ; Save changes to page setup?
          "Yes"                                ; Proceed with plot
        )

        (setq i (1+ i))
      )

      (prompt "\nAll rectangles plotted successfully.")
    )
    (prompt "\nNo rectangles selected.")
  )

  (princ)
)