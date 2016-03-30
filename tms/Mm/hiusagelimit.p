/* ----------------------------------------------------------------------
  MODULE .......: HiusageLimit
  TASK .........: UPDATEs table HiusageLimit
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 21.05.02/tk Event logging added
                  28.02.03 tk tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhHiusageLimit AS HANDLE NO-UNDO.
   lhHiusageLimit = BUFFER HiusageLimit:HANDLE.
   RUN StarEventInitialize(lhHiusageLimit).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhHiusageLimit).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF INPUT PARAMETER  HiusageCategory LIKE  HiusageLimit.Category .
DEF VAR BillCode      LIKE HiusageLimit.BillCode        NO-UNDO.
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
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR ProductName     AS C FORMAT "X(25)"  NO-UNDO.

form
   HiusageLimit.Category  
   HiusageLimit.BillCode
   HELP "TOTAL=Total amount of calls"
   productname 
   HiusageLimit.Limit 
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  HIGHUSAGE CATEGORY MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
   HiusageLimit.Category  
   HiusageLimit.BillCode
   HELP "TOTAL=Total amount of calls"
   HiusageLimit.Limit 

            /* LABEL FORMAT */

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek  HiusageLimit */
    Hiusagecategory
    HELP "Enter Code of Categorye "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.



cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST HiusageLimit
WHERE (IF HiusageCategory = "" THEN TRUE ELSE 
hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
IF AVAILABLE HiusageLimit THEN ASSIGN
   Memory       = recid(HiusageLimit)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No Highusage category items available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a HiusageLimit  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR HiusageLimit.Category.
           IF INPUT FRAME lis HiusageLimit.Category = "" THEN 
           LEAVE add-row.
           
           CREATE HiusageLimit.
           ASSIGN
           HiusageLimit.Category = INPUT FRAME lis HiusageLimit.Category.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhHiusageLimit).

           ASSIGN
           Memory = recid(HiusageLimit)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST HiusageLimit
      WHERE (IF HiusageCategory = "" THEN TRUE ELSE hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE HiusageLimit THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND HiusageLimit WHERE recid(HiusageLimit) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE HiusageLimit THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(HiusageLimit).
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
        PAUSE 0 NO-MESSAGE.

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
        ufk[1]= 35  ufk[2]= 0 ufk[3]= 0  ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW HiusageLimit.Category {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) HiusageLimit.Category WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW HiusageLimit.BillCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) HiusageLimit.BillCode WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND HiusageLimit WHERE recid(HiusageLimit) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE HiusageLimit THEN
              ASSIGN FIRSTrow = i Memory = recid(HiusageLimit).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE HiusageLimit THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(HiusageLimit)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE HiusageLimit THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(HiusageLimit).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND HiusageLimit WHERE recid(HiusageLimit) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE HiusageLimit THEN DO:
           Memory = recid(HiusageLimit).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE HiusageLimit THEN Memory = recid(HiusageLimit).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
        END.
     END. /* PREVious page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND HiusageLimit WHERE recid(HiusageLimit) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET HiusageLimit WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF HiusageCategory ENTERED THEN DO:
          FIND FIRST HiusageLimit WHERE 
                     HiusageLimit.Category >= hiusagecategory AND 
          (IF HiusageCategory = "" THEN TRUE 
          ELSE hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
          IF NOT AVAILABLE HiusageLimit THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some HiusageLimit/HiusageLimit was found */
          ASSIGN order = 1 Memory = recid(HiusageLimit) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

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
          HiusageLimit.Category  
          HiusageLimit.Limit 
          HiusageLimit.BillCode.


       RUN local-find-NEXT.
       IF AVAILABLE HiusageLimit THEN Memory = recid(HiusageLimit).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE HiusageLimit THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(HiusageLimit).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       HiusageLimit.Category  
       HiusageLimit.Limit 
       HiusageLimit.BillCode.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhHiusageLimit).

           DELETE HiusageLimit.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST HiusageLimit
           WHERE (IF HiusageCategory = "" 
           THEN TRUE ELSE hiusagelimit.category = Hiusagecategory)) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhHiusageLimit).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY
       HiusageLimit.Category  
       HiusageLimit.Limit 
       HiusageLimit.BillCode.
       

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhHiusageLimit).

       RUN local-disp-row.
       xrecid = recid(HiusageLimit).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(HiusageLimit) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(HiusageLimit) must-print = TRUE.
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
      FIND HiusageLimit WHERE recid(HiusageLimit) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND HiusageLimit WHERE recid(HiusageLimit) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST HiusageLimit
       WHERE (IF HiusageCategory = "" THEN TRUE ELSE hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST  HiusageLimit 
       WHERE (IF HiusageCategory = "" THEN TRUE ELSE hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST HiusageLimit
       WHERE (IF HiusageCategory = "" THEN TRUE ELSE hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST HiusageLimit
       WHERE (IF HiusageCategory = "" THEN TRUE ELSE hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT HiusageLimit
       WHERE (IF HiusageCategory = "" THEN TRUE ELSE hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT HiusageLimit
       WHERE (IF HiusageCategory = "" THEN TRUE ELSE hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV HiusageLimit
       WHERE (IF HiusageCategory = "" THEN TRUE ELSE hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV HiusageLimit
       WHERE (IF HiusageCategory = "" THEN TRUE ELSE hiusagelimit.category = Hiusagecategory) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       HiusageLimit.Category
       ProductName
       HiusageLimit.Limit 
       HiusageLimit.BillCode
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND BillItem WHERE 
        BillItem.Brand    = gcBrand AND 
        BillItem.BillCode = HiusageLimit.BillCode NO-LOCK NO-ERROR.

   IF avail BillItem then ProductName = billItem.BIName.
   ELSE IF HiusageLimit.BillCode = "TOTAL" 
     then ProductName = "TOTAL VALUE OF CALLS".
   ELSE Productname   = "".   

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      HiusageLimit.Category  
      HiusageLimit.BillCode
      HiusageLimit.Limit 
      HiusageLimit.BillCode
      productname 

      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
         UPDATE
         HiusageLimit.BillCode
         HiusageLimit.Limit 
         
         WITH FRAME lis.
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

