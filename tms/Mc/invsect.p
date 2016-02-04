/* ----------------------------------------------------------------------
  MODULE .......: InvSect.P
  TASK .........: UPDATE Invoice Sections
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 25-10-99
  CHANGED ......: 28.10.99 pt 'calls' FIELD added
                  20.05.02/tk Event logging added
                  21.05.02/tk invoice texts
                  28.02.03/tk tokens
                  17.03.03/aam PrintHead
                  05.09.03/aam brand
                  20.12.06/aam translations (invlang)
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable InvSect

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invsect'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhInvSect AS HANDLE NO-UNDO.
   lhInvSect = BUFFER InvSect:HANDLE.
   RUN StarEventInitialize(lhInvSect).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhInvSect).
   END.
END.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR InvSect  LIKE InvSect.InvSect  NO-UNDO.
DEF VAR ISName LIKE InvSect.ISName NO-UNDO.
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

form
    InvSect.Brand 
    InvSect.InvSect      /* COLUMN-LABEL FORMAT */
    InvSect.ISName     /* COLUMN-LABEL FORMAT */
    InvSect.PrintHead
             /* COLUMN-LABEL FORMAT */
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Invoice Sections "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    InvSect.Brand
    InvSect.InvSect     /* LABEL FORMAT */
    InvSect.ISName    /* LABEL FORMAT */
    InvSect.PrintHead        /* LABEL FORMAT */

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

{Func/brand.i}

form /* seek Invoice Section  BY  InvSect */
    "Brand:" lcBrand skip
    "Code :" InvSect
    HELP "Enter Code of Invoice Section"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Invoice Section  BY ISName */
    "Brand:" lcBrand skip
    "Name :" ISName
    HELP "Enter Name of Invoice Section"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code of Section,By Name of Section,By 3, By 4".


FIND FIRST InvSect
WHERE InvSect.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE InvSect THEN ASSIGN
   Memory       = recid(InvSect)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No invoice sections available !" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a InvSect  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis
        ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW:

           CLEAR FRAME lis NO-PAUSE.

           DISPLAY lcBrand @ InvSect.Brand.
           PROMPT-FOR InvSect.InvSect
           VALIDATE
              (InvSect.InvSect NOT ENTERED OR
              NOT CAN-FIND(InvSect WHERE    
                           InvSect.Brand = lcBrand AND
                           InvSect.InvSect = INPUT InvSect.InvSect),
              "Invoice Section " + string(INPUT InvSect.InvSect) +
              " already exists !").
           IF INPUT FRAME lis InvSect.InvSect NOT ENTERED THEN 
           LEAVE add-row.
           CREATE InvSect.
           ASSIGN
           InvSect.Brand   = lcBrand 
           InvSect.InvSect = INPUT FRAME lis InvSect.InvSect.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhInvSect).

           ASSIGN
           Memory = recid(InvSect)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST InvSect
      WHERE InvSect.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE InvSect THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND InvSect WHERE recid(InvSect) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE InvSect THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(InvSect).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 814
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 1760 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW InvSect.InvSect ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvSect.InvSect WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW InvSect.ISName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvSect.ISName WITH FRAME sel.
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
        FIND InvSect WHERE recid(InvSect) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE InvSect THEN
              ASSIGN FIRSTrow = i Memory = recid(InvSect).
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
           IF NOT AVAILABLE InvSect THEN DO:
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
                rtab[1] = recid(InvSect)
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
           IF NOT AVAILABLE InvSect THEN DO:
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
              rtab[FRAME-DOWN] = recid(InvSect).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND InvSect WHERE recid(InvSect) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE InvSect THEN DO:
           Memory = recid(InvSect).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE InvSect THEN Memory = recid(InvSect).
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
           FIND InvSect WHERE recid(InvSect) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              InvSect WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF InvSect ENTERED THEN DO:
          FIND FIRST InvSect WHERE 
             InvSect.Brand = lcBrand AND
             InvSect.InvSect >= InvSect
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              ISName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF ISName ENTERED THEN DO:
          FIND FIRST InvSect USE-INDEX ISName WHERE 
             InvSect.Brand = lcBrand AND
             InvSect.ISName >= ISName
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* translations */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  
        FIND InvSect WHERE RECID(InvSect) = rtab[FRAME-LINE] NO-LOCK.
        RUN Mc/invlang(8,InvSect.InvSect).
          
        ufkey = TRUE.
        NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       FIND FIRST BillItem WHERE 
                  BillItem.Brand   = InvSect.Brand AND
                  BillItem.InvSect = InvSect.InvSect 
       NO-LOCK NO-ERROR.

       IF AVAIL BillItem THEN DO:
          MESSAGE 
          "There are Product(s) with this"  SKIP
          "Section Code - change BillCode"   SKIP
          "records before You delete this"  SKIP
          "Invoice Section Record"
          VIEW-AS ALERT-BOX 
          TITLE " DELETE NOT ALLOWED ".
          RETURN.
       END.   

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       InvSect.InvSect InvSect.ISName .

       RUN local-find-NEXT.
       IF AVAILABLE InvSect THEN Memory = recid(InvSect).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE InvSect THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(InvSect).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       InvSect.InvSect InvSect.ISName .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhInvSect).

           DELETE InvSect.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST InvSect
           WHERE InvSect.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:
        FIND InvSect WHERE recid(InvSect) = rtab[FRAME-line(sel)] NO-LOCK.
        RUN Mc/invotxt("InvSect", InvSect.InvSect).
        ASSIGN Memory = recid(InvSect) must-print = TRUE ufkey = TRUE.

     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvSect).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY InvSect.InvSect.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeMOdifyEvent(lhInvSect).

       RUN local-disp-row.
       xrecid = recid(InvSect).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(InvSect) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(InvSect) must-print = TRUE.
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
      FIND InvSect WHERE recid(InvSect) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND InvSect WHERE recid(InvSect) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST InvSect
       WHERE InvSect.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST InvSect USE-INDEX ISName
       WHERE InvSect.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST InvSect
       WHERE InvSect.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST InvSect USE-INDEX ISName
       WHERE InvSect.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT InvSect
       WHERE InvSect.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT InvSect USE-INDEX ISName
       WHERE InvSect.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV InvSect
       WHERE InvSect.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV InvSect USE-INDEX ISName
       WHERE InvSect.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       InvSect.Brand
       InvSect.InvSect
       InvSect.ISName
       InvSect.PrintHead
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          InvSect.Brand
          InvSect.ISName
          InvSect.PrintHead
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
         UPDATE
             InvSect.ISName
             InvSect.PrintHead
         WITH FRAME lis.
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

