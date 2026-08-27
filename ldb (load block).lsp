;;; ================================================================
;;; LDB - Load toan bo block tu 1 file DWG chi dinh vao ban ve hien hanh
;;; Nguyen ly: dung ObjectDBX doc file nguon (khong can mo file),
;;;            copy tat ca dinh nghia block sang ban ve dang mo.
;;; Dac diem:
;;;   - Khong can mo file nguon, khong anh huong file nguon
;;;   - Tu bo qua: layout (*Model_Space/*Paper_Space), xref,
;;;     block an danh (*U...), block da ton tai trung ten
;;;   - Bao cao ro: bao nhieu block nap moi, bao nhieu bo qua
;;; Cach dung: go lenh LDB -> chon file DWG
;;; ================================================================

(vl-load-com)

;; ---------- Mo file DWG bang ObjectDBX ----------
;; Tra ve doi tuong dbx neu OK, nil neu loi
(defun ldb:open-dbx (path / acad ver dbx rs)
  (setq acad (vlax-get-acad-object)
        ver  (itoa (fix (atof (getvar "ACADVER"))))
  )
  ;; Thu dang co version truoc (vd ObjectDBX.AxDbDocument.25)
  (setq rs (vl-catch-all-apply
             'vla-GetInterfaceObject
             (list acad (strcat "ObjectDBX.AxDbDocument." ver))))
  (if (vl-catch-all-error-p rs)
    (setq rs (vl-catch-all-apply
               'vla-GetInterfaceObject
               (list acad "ObjectDBX.AxDbDocument")))
  )
  (if (vl-catch-all-error-p rs)
    (progn (prompt "\n*** Khong khoi tao duoc ObjectDBX! ***") nil)
    (progn
      (setq dbx rs
            rs  (vl-catch-all-apply 'vla-Open (list dbx path)))
      (if (vl-catch-all-error-p rs)
        (progn
          (prompt (strcat "\n*** Khong doc duoc file: " path " ***"))
          (vlax-release-object dbx)
          nil
        )
        dbx
      )
    )
  )
)

;; ---------- Kiem tra block dinh nghia "that" (khong phai layout/xref/an danh) ----------
(defun ldb:real-block-p (blkdef / name)
  (setq name (vla-get-Name blkdef))
  (and
    (/= (substr name 1 1) "*")                          ; bo an danh + layout cu
    (= :vlax-false (vla-get-IsLayout blkdef))           ; bo layout
    (= :vlax-false (vla-get-IsXRef blkdef))             ; bo xref
  )
)

;; ================================================================
;; LENH CHINH
;; ================================================================
(defun c:LDB (/ path curdoc dbx blks copylist skiplist name n rs)
  (setq curdoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; ----- Chon file nguon -----
  (setq path (getfiled "Chon file DWG chua block can nap" "" "dwg" 16))
  (cond
    ((not path)
     (prompt "\n* Da huy lenh. *")
    )
    ((= (strcase path) (strcase (vla-get-FullName curdoc)))
     (prompt "\n*** File nguon trung voi ban ve dang mo! ***")
    )
    ((not (setq dbx (ldb:open-dbx path)))
     ;; thong bao loi da in trong ldb:open-dbx
    )
    (t
     ;; ----- Duyet block trong file nguon -----
     (setq blks     (vla-get-Blocks dbx)
           copylist '()
           skiplist '())
     (vlax-for blkdef blks
       (if (ldb:real-block-p blkdef)
         (progn
           (setq name (vla-get-Name blkdef))
           (if (tblsearch "BLOCK" name)
             (setq skiplist (cons name skiplist))      ; da co -> bo qua
             (setq copylist (cons blkdef copylist))    ; chua co -> copy
           )
         )
       )
     )

     ;; ----- Copy sang ban ve hien hanh -----
     (if copylist
       (progn
         (setq rs (vl-catch-all-apply
                    'vlax-invoke
                    (list dbx 'CopyObjects copylist (vla-get-Blocks curdoc))))
         (if (vl-catch-all-error-p rs)
           (prompt (strcat "\n*** Loi khi copy block: "
                           (vl-catch-all-error-message rs) " ***"))
           (progn
             (setq n (length copylist))
             (prompt (strcat "\n==> Da nap " (itoa n) " block tu file:"))
             (foreach b (reverse copylist)
               (prompt (strcat "\n  + " (vla-get-Name b)))
             )
           )
         )
       )
       (prompt "\n* Khong co block moi nao de nap. *")
     )

     ;; ----- Bao cac block bi bo qua vi trung ten -----
     (if skiplist
       (progn
         (prompt (strcat "\n* Bo qua " (itoa (length skiplist))
                         " block da ton tai trong ban ve (giu nguyen ban hien tai):"))
         (foreach s (reverse skiplist) (prompt (strcat "\n  - " s)))
         (prompt "\n  (Muon lay ban moi tu file nguon: xoa het block do trong ban ve, PURGE, roi chay lai LDB)")
       )
     )

     ;; ----- Dong ObjectDBX -----
     (vlax-release-object dbx)
    )
  )
  (princ)
)

(prompt "\nDa nap lenh LDB - Load toan bo block tu file DWG chi dinh vao ban ve hien hanh.")
(princ)
