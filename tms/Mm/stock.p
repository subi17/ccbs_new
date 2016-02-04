
/* ----------------------------------------------------------------------
  MODULE .......: Stock.P
  TASK .........: UPDATE Stock Locations
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 01-06-99
  CHANGED ......: 30.06.99 pt xfile1, xfile2
                  07.10.99 jp urights added
                  21.05.02/tk Event logging added
                  09.09.02/jp Validation....
                  07.03.03/tk tokens
                  04.09.03 jp brand
                  14.04.04 jp delistat
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'stock'}
{Syst/tmsconst.i}

&GLOBAL-DEFINE BrTable Stock

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhStock AS HANDLE NO-UNDO.
   lhStock = BUFFER Stock:HANDLE.
   RUN StarEventInitialize(lhStock).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhStock).
   END.
END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Stock  LIKE Stock.Stock  NO-UNDO.
DEF VAR StoName LIKE Stock.StoName NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"  NO-UNDO.
DEF VAR deliname     AS C                    No-UNDO format "X(20)".
DEF VAR DeliNames    AS C                    NO-UNDO.
DEF VAR lcError      AS CHAR NO-UNDO.

DeliNames =
"External txt file for requests,LOW Priority Batch,HI priority Batch,,".


form
    Stock.Brand 
    Stock.Stock FORMAT "x(12)"      /* COLUMN-LABEL FORMAT */
    Stock.StoName  format "x(20)"
    Stock.StoType
    Stock.ZipCodeExp format "x(20)"
             /* COLUMN-LABEL FORMAT */
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " Stock Locations "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    Stock.Stock format "x(12)"     /* LABEL FORMAT */
    Stock.StoName    /* LABEL FORMAT */
     VALIDATE(INPUT stock.stoname ne "","Missing Stock Name!")  /*  skip(1) */
    Stock.StoType HELP {&ICC_STOCK_TYPES}
    Stock.ZipCodeExp format "x(20)" HELP
    "Wildcards: * = group of chars or empty group . = single char"
    Stock.StoStreet
    Stock.StoCity
    Stock.Reseller
    Reseller.RsName
    Stock.SimDel
    deliname
    Stock.StoFile1
    Stock.StoFile2
    Stock.StoFile3
    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 1 col
    FRAME lis.

form /* seek Stock  BY  Stock */
    "Brand Code:" lcBrand  HELP "Enter Brand  " 
     VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Stock Code:"  Stock   help "Enter Stock Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Stock  BY StoName */
    "Brand Code:" lcBrand  HELP "Enter Brand  "
     VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Stock Name:" StoName help "Enter Stock Name"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".



FIND FIRST Stock 
WHERE Stock.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Stock THEN ASSIGN
   Memory       = recid(Stock)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No stocks available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.
LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 37 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a Stock  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSACTION:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR Stock.Stock
           VALIDATE
              (Stock.Stock = "" OR
              NOT CAN-FIND(Stock using  Stock.Stock),
              "Stock " + string(INPUT Stock.Stock) +
              " already exists !").
           IF input Stock.Stock = "" THEN LEAVE ADD-ROW.
           CREATE Stock.
           ASSIGN
           Stock.Brand = lcBrand 
           Stock.Stock = INPUT FRAME lis Stock.Stock.

           RUN local-UPDATE-record.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhStock).

           ASSIGN
           Memory = recid(Stock)
           xrecid = Memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST Stock
      WHERE Stock.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Stock THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Stock WHERE recid(Stock) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Stock THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Stock).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 207
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Stock.Stock ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Stock.Stock WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Stock.StoName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Stock.StoName WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND Stock WHERE recid(Stock) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE Stock THEN
              ASSIGN FIRSTrow = i Memory = recid(Stock).
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
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE Stock THEN DO:
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
                rtab[1] = recid(Stock)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE Stock THEN DO:
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
              rtab[FRAME-DOWN] = recid(Stock).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Stock WHERE recid(Stock) = Memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE Stock THEN DO:
           Memory = recid(Stock).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE Stock THEN Memory = recid(Stock).
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
           Memory = rtab[FRAME-DOWN].
           FIND Stock WHERE recid(Stock) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       Stock = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       DISP lcBrand With Frame f1.
       UPDATE lcBrand WHEN gcAllBrand = TRUE Stock WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF Stock <> "" THEN DO:

          if lcBrand ne "*" THEN 
          FIND FIRST Stock WHERE Stock.Stock >= Stock AND
                     Stock.Brand = lcBrand 
          /* srule */ NO-LOCK NO-ERROR.
          ELSE 
          FIND FIRST Stock WHERE Stock.Stock >= Stock 
          NO-LOCK NO-ERROR. 

          IF NOT fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       StoName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       Disp lcBrand With FRAME f2.
       UPDATE lcBrand WHEN gcAllBrand = TRUE 
              StoName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF StoName <> "" THEN DO:
          FIND FIRST Stock USE-INDEX stoname WHERE Stock.StoName >= StoName
           AND Stock.Brand = lcBrand NO-LOCK NO-ERROR.

           IF NOT fRecFound(2) THEN NEXT.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* CONTAINS ... */
       ufkey = TRUE.              
       RUN local-find-this (FALSE).
       RUN Mm/stobal2.p(Stock.Stock).
       NEXT loop.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       Stock.Stock Stock.StoName Stock.Brand.

       RUN local-find-NEXT.
       IF AVAILABLE Stock THEN Memory = recid(Stock).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-prev.
          IF AVAILABLE Stock THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(Stock).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       Stock.Stock Stock.StoName Stock.Brand.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhStock).

           DELETE Stock.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST Stock
           WHERE Stock.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSACTION:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhStock).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       CLEAR FRAME lis no-pause.
       DISPLAY Stock.Stock.
       RUN local-UPDATE-record.
       
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhStock).

       HIDE FRAME lis NO-PAUSE.
       RUN local-disp-row.
       xrecid = recid(Stock).
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Stock) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Stock) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS lo NO-UNDO.
    IF exlock THEN
      FIND Stock WHERE recid(Stock) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND Stock WHERE recid(Stock) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST Stock
       WHERE Stock.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST Stock USE-INDEX StoName
       WHERE Stock.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST Stock
       WHERE Stock.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST Stock USE-INDEX StoName
       WHERE Stock.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT Stock
       WHERE Stock.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT Stock USE-INDEX StoName
       WHERE Stock.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev Stock
       WHERE Stock.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev Stock USE-INDEX StoName
       WHERE Stock.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       DISPLAY
       Stock.Stock
       Stock.Brand
       Stock.StoName
       Stock.StoType
       Stock.ZipCodeExp

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND ReSeller where Reseller.Reseller = Stock.Reseller no-lock no-error.

   Deliname = ENTRY(stock.SimDel + 1,DeliNames).

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   RUN local-find-others.
   DISP
       Stock.StoName
       Stock.StoStreet
       Stock.StoCity
       Stock.Reseller
       Stock.SimDel
       DeliName
       Stock.StoFile1
       Stock.StoFile2
       Stock.StoFile3
       Stock.StoType
       Stock.ZipCodeExp
   WITH FRAME lis.

   IF lcRight = "RW" THEN DO:
      UPDATE
          Stock.StoName
          Stock.StoType
          Stock.ZipCodeExp
          Stock.StoStreet
          Stock.StoCity
          Stock.Reseller
          Stock.SimDel
          Stock.StoFile1
          Stock.StoFile2
          Stock.StoFile3

      WITH FRAME lis EDITING:
          READKEY.
          IF KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
          IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
             PAUSE 0.
             IF FRAME-FIELD = "Reseller" and input Stock.Reseller ne "" THEN DO:
                FIND ReSeller WHERE Reseller.Reseller =
                INPUT FRAME lis Stock.Reseller NO-LOCK NO-ERROR.
                IF NOT AVAIL ReSeller THEN DO:
                   BELL.
                   MESSAGE "Unknown retailer !".
                   NEXT.
                END.
                DISP Reseller.RsName WITH FRAME lis.
             END.
             ELSE IF FRAME-FIELD = "simdel" THEN DO:
                  IF INPUT stock.SimDel > 2 THEN DO:
                  MESSAGE
                  "Invalid Delivery Code.  Correct values are: " SKIP
                  "0: Append Delivery Requests into External file one by one"
                  SKIP              
                  "1: Batch method, Low priority by default" SKIP
                  "2: Batch method, HI priority by default"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
                  END.
             END.
             IF FRAME-FIELD = "StoType" AND INPUT Stock.StoType NE "" THEN DO:
                IF LOOKUP(INPUT Stock.StoType, {&ICC_STOCK_TYPES}) = 0 THEN DO:
                  MESSAGE "Unknown stock type" VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               DISP UPPER(INPUT Stock.StoType) @ Stock.StoType.
             END.
             IF FRAME-FIELD = "ZipCodeExp" THEN DO:
               
               lcError = "".
               DO i = 1 TO LENGTH(INPUT Stock.ZipCodeExp):
                  IF INDEX(",0123456789*.",
                     SUBSTRING(INPUT Stock.ZipCodeExp,i,1)) = 0 THEN DO:
                     lcError = 
                        SUBST("Expression cannot contain character &1",
                        SUBSTRING(INPUT Stock.ZipCodeExp, i, 1)).
                     LEAVE. 
                  END.
               END.
               
               IF lcError = "" AND
                  LENGTH(INPUT Stock.ZipCodeExp) > 0 AND INDEX("0123456789",
                  SUBSTRING(INPUT Stock.ZipCodeExp,1,1)) = 0 THEN 
                  lcError = "Expression must start with number".

               IF lcError NE "" THEN DO:
                  MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
             END.
          END.
          APPLY LASTKEY.
       END. /* EDITING */
   END.
   ELSE PAUSE.
END PROCEDURE.

