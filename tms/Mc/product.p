/* ----------------------------------------------------------------
  MODULE .......: Product.P
  TASK .........: UPDATE Products
  APPLICATION ..: 
  AUTHOR .......: tk
  CREATED ......: 08-05-02
  CHANGED ......: 13-05-02 tk eventlogging added
                  21.05.02/tk InvText
                  06.02.04 jp custnum for memo
  ---------------------------------------------------------------------- */

{commali.i}
{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'product'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhProduct AS HANDLE NO-UNDO.
   lhProduct = BUFFER Product:HANDLE.
   RUN StarEventInitialize(lhProduct).

   DEFINE VARIABLE lhPPItem AS HANDLE NO-UNDO.
   lhPPItem = BUFFER PPItem:HANDLE.
   RUN StarEventInitialize(lhPPItem).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhProduct).
   END.


END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Product     LIKE Product.Product         NO-UNDO.
DEF VAR ProdName   LIKE Product.ProdName       NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR MaxOrder     AS i                      NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"  NO-UNDO.

form
    Product.Product       /* COLUMN-LABEL FORMAT */
    Product.ProdName     /* COLUMN-LABEL FORMAT */
    WITH width 80 OVERLAY SCROLL 1 15 DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " Products "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    Product.Product      /* LABEL FORMAT */
    Product.ProdName    /* LABEL FORMAT */
    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 1 columns
    FRAME lis.

form /* seek Product  BY  Product */
    Product
    help "Enter Product Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Product BY ProdName */
    ProdName
    help "Enter Product Name"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".
FIND FIRST Product
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE Product THEN ASSIGN
   memory       = recid(Product)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No products available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.
LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order THEN DO:
       pr-order = order.
       PUT SCREEN ROW 19 col 37 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a Product  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN ufcolor.
ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.
        DO TRANSAction:
           PROMPT-FOR Product.Product
           VALIDATE
              (Product.Product = "" OR
              NOT CAN-FIND(Product using  Product.Product),
              "Product " + string(INPUT Product.Product) +
              " already exists !").
           IF input Product.Product = "" THEN LEAVE ADD-ROW.
           CREATE Product.
           ASSIGN
           Product.Product = INPUT FRAME lis Product.Product.
           UPDATE Product.ProdName.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhProduct).

           ASSIGN
           memory = recid(Product)
           xrecid = memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST Product
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Product THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND Product WHERE recid(Product) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Product THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Product).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 no-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 251 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 1760 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Product.Product ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Product.Product WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Product.ProdName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Product.ProdName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND Product WHERE recid(Product) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE Product THEN
              ASSIGN FIRSTrow = i memory = recid(Product).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND Product WHERE recid(Product) = rtab[FRAME-LINE] NO-LOCK.
           RUN local-find-prev.
           IF NOT AVAILABLE Product THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(Product)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND Product WHERE recid(Product) = rtab[FRAME-DOWN] NO-LOCK .
           RUN local-find-NEXT.
           IF NOT AVAILABLE Product THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(Product).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND Product WHERE recid(Product) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE Product THEN DO:
           memory = recid(Product).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE Product THEN memory = recid(Product).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
        END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND Product WHERE recid(Product) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       Product = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE Product WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF Product <> "" THEN DO:
          FIND FIRST Product WHERE Product.Product >= Product
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Product THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some product/ProdId was found */
          ASSIGN order = 1 memory = recid(Product) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ProdName = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE ProdName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF ProdName <> "" THEN DO:
          FIND FIRST Product WHERE Product.ProdName >= ProdName
          USE-INDEX ProdName /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Product THEN DO:
             bell. MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some product/ProdName was found */
          ASSIGN order = 2 memory = recid(Product) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO TRANS:  /* Within ProdPack */

       FIND Product WHERE recid(Product) = rtab[FRAME-line(sel)].
       RUN ppcomp2(Product.Product).
       ufkey = TRUE.
       NEXT LOOP.
     END.

     IF LOOKUP(nap,"4,F4") > 0 THEN DO TRANS: /* memo */
       FIND Product WHERE RECID(Product) = rtab[FRAME-LINE(sel)]
       NO-LOCK NO-ERROR.
       RUN memo(INPUT 0,
                INPUT "PRODUCT",
                INPUT STRING(Product.Product),
                INPUT "Product Id").
       ufkey = TRUE.
       NEXT.
     END.

     IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       FIND Product WHERE recid(Product) = rtab[FRAME-LINE] NO-LOCK.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       Product.Product Product.ProdName /* sd */.

       RUN local-find-NEXT.
       IF AVAILABLE Product THEN memory = recid(Product).
       ELSE DO:
          /* read back the record that is TO be  removed */
          FIND Product WHERE recid(Product) = rtab[FRAME-LINE] NO-LOCK.
          /* THEN previous record */
          RUN local-find-prev.

          IF AVAILABLE Product THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(Product).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       FIND Product WHERE recid(Product) = rtab[FRAME-LINE]
       EXCLUSIVE-LOCK.

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       Product.Product Product.ProdName.
       IF ok THEN DO:
           IF CAN-FIND(FIRST PPItem WHERE
                             PPItem.ProdPack = Product.Product) THEN DO:
              ok = FALSE.
              MESSAGE "This Product is a part of prodpack(s)" SKIP
                      " - still delete ? "
              VIEW-AS ALERT-BOX
              BUTTONS YES-NO UPDATE ok.

           END.       
       END.

       IF ok THEN DO:     
           FOR EACH PPItem WHERE
                    PPItem.ProdPack = Product.Product:

              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPPItem).

              DELETE PPItem.
           END.

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhProduct).

           DELETE Product.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST Product
           /* srule */) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF lookup(nap,"7,f7") > 0 THEN DO:
        FIND Product WHERE recid(Product) = rtab[FRAME-line(sel)] NO-LOCK.
        RUN invotxt("Product",Product.Product).
        ASSIGN memory = recid(Product) must-print = TRUE ufkey=true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction:
       /* change */
       FIND Product WHERE recid(Product) = rtab[FRAME-line(sel)]
       EXCLUSIVE-LOCK.
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.
       cfc = "lis". RUN ufcolor.
       DISPLAY 
          Product.Product
          Product.ProdName.
       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhProduct).

          UPDATE Product.ProdName.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhProduct).
       END.
       ELSE PAUSE.

       HIDE FRAME lis NO-PAUSE.
       DISPLAY Product.ProdName
       WITH FRAME sel.
       xrecid = recid(Product).


     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(Product) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"end,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(Product) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST Product
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST Product USE-INDEX ProdName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST Product
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST Product USE-INDEX ProdName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT Product
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT Product USE-INDEX ProdName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev Product
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev Product USE-INDEX ProdName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       DISPLAY
       Product.Product Product.ProdName
       WITH FRAME  sel.
END PROCEDURE.

