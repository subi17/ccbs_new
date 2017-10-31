/* ----------------------------------------------------------------------
  MODULE .......: CustTemp
  TASK .........: UPDATEs table CustTemp
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 05.11.02 jr Eventlog
                  07.03.03 tk tokens
                  15.09.03 jp Brand 
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable custtemp

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CustTemp'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR TemplNum      LIKE CustTemp.TemplNum        NO-UNDO.
DEF VAR TemplName    LIKE CustTemp.TemplName        NO-UNDO.
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

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustTemp AS HANDLE NO-UNDO.
   lhCustTemp = BUFFER CustTemp:HANDLE.
   RUN StarEventInitialize(lhCustTemp).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhCustTemp).
   END.
END.

form
    CustTemp.Brand         FORMAT "X(5)"
    CustTemp.TemplNum      column-label "Code" /*  FORMAT */
    CustTemp.TemplName    column-label "Name" /* FORMAT */
    CustTemp.CustNum
    Customer.CustName     format "x(14)"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.CUICommon:cfc)   
    TITLE COLOR VALUE(Syst.CUICommon:ctc) " " + Syst.CUICommon:ynimi +
    "  TEMPLATE MENU  "
    + string(TODAY,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    CustTemp.TemplNum      COLumn-label "Code"
    CustTemp.TemplName    Column-label "Name"  /* FORMAT */
    CustTemp.CustNum
            /* LABEL FORMAT */

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek  TemplNum */
   "Brand Code:" lcBrand  HELP "Enter Brand"
   VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
   "Template..:"  TemplNum
    HELP "Enter Code of  Template "
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND CODE "
    COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  TemplName */
   "Brand Code:" lcBrand  HELP "Enter Brand"
   VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
   "Name......:"  TemplName
    HELP "Enter Name of the Template "
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND Name "
    COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f2.

form
    CustTemp.Memo

    WITH OVERLAY ROW 3 centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc)
    " Memo: " + CustTemp.TemplName + " " WITH NO-LABELS 1 columns
    FRAME f4.


Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST CustTemp
WHERE CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE CustTemp THEN ASSIGN
   Memory       = recid(CustTemp)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No customer templates available !" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a CustTemp  */
      ASSIGN Syst.CUICommon:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR CustTemp.TemplNum
           VALIDATE
              (CustTemp.TemplNum NOT ENTERED OR
              NOT CAN-FIND(CustTemp using  CustTemp.TemplNum WHERE 
              custTemp.Brand = lcBrand ),
              "Template Code " + string(INPUT CustTemp.TemplNum) +
              " already exists !").
           IF INPUT FRAME lis CustTemp.TemplNum = "" THEN 
           LEAVE add-row.
           CREATE CustTemp.
           ASSIGN
           CustTemp.Brand    = lcBrand 
           CustTemp.TemplNum = INPUT FRAME lis CustTemp.TemplNum.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustTemp).
           ASSIGN
           Memory = recid(CustTemp)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CustTemp
      WHERE CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CustTemp THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CustTemp WHERE recid(CustTemp) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CustTemp THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CustTemp).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0  ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CustTemp.TemplNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.CUICommon:ccc) CustTemp.TemplNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW CustTemp.TemplName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.CUICommon:ccc) CustTemp.TemplName WITH FRAME sel.
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
        FIND CustTemp WHERE recid(CustTemp) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CustTemp THEN
              ASSIGN FIRSTrow = i Memory = recid(CustTemp).
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
           IF NOT AVAILABLE CustTemp THEN DO:
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
                rtab[1] = recid(CustTemp)
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
           IF NOT AVAILABLE CustTemp THEN DO:
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
              rtab[FRAME-DOWN] = recid(CustTemp).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CustTemp WHERE recid(CustTemp) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CustTemp THEN DO:
           Memory = recid(CustTemp).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CustTemp THEN Memory = recid(CustTemp).
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
           FIND CustTemp WHERE recid(CustTemp) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       disp lcbrand WITH FRAME f1.
       SET  lcBrand WHEN Syst.CUICommon:gcAllBrand =  TRUE 
            TemplNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF TemplNum ENTERED THEN DO:
          FIND FIRST CustTemp WHERE 
                     CustTemp.TemplNum >= TemplNum   AND 
                     CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.

           IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       DISP lcBrand WITH FRAME f2.
       SET  lcBrand WHEN Syst.CUICommon:gcAllBrand TRUE
            TemplName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF TemplName ENTERED THEN DO:
          FIND FIRST CustTemp WHERE 
                     CustTemp.TemplName >= TemplName AND 
                     CustTemp.Brand = lcBrand 
          NO-LOCK NO-ERROR.

           IF NOT  fRecFound(2) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO TRANS ON ENDKEY UNDO, NEXT LOOP:

        Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
        ehto = 9. 
        ufkey = TRUE.
        RUN local-find-this(TRUE).
        DISP   CustTemp.Memo WITH FRAME f4.
        IF lcRight = "RW" THEN DO:
           RUN Syst/ufkey.p.
           UPDATE CustTemp.Memo WITH FRAME f4.
        END.
        ELSE PAUSE.
        HIDE FRAME f4 NO-PAUSE.
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
       COLOR DISPLAY VALUE(Syst.CUICommon:ctc)
       CustTemp.TemplNum CustTemp.TemplName Custtemp.bRAnd .

       RUN local-find-NEXT.
       IF AVAILABLE CustTemp THEN Memory = recid(CustTemp).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CustTemp THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CustTemp).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.CUICommon:ccc)
       CustTemp.TemplNum CustTemp.TemplName  CustTemp.CustNum.
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCustTemp).
           DELETE CustTemp.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CustTemp
           WHERE CustTemp.Brand = lcBrand) THEN DO:
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
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       Syst.CUICommon:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY 
          CustTemp.TemplNum
          CustTemp.TemplName 
          CustTemp.CustNum.

       IF lcRight = "RW" THEN DO:

          RUN Syst/ufkey.p. 
          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustTemp). 
          RUN local-UPDATE-record.                                  

          /* IF  User Wanted TO Cancel this Change TRANSACTION */
          IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
          KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustTemp).

       END.
       ELSE PAUSE.

       HIDE FRAME lis NO-PAUSE.

       RUN local-disp-row.
       xrecid = recid(CustTemp).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CustTemp) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CustTemp) must-print = TRUE.
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
      FIND CustTemp WHERE recid(CustTemp) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CustTemp WHERE recid(CustTemp) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CustTemp
       WHERE CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST CustTemp USE-INDEX TemplName
       WHERE CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CustTemp
       WHERE CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST CustTemp USE-INDEX TemplName
       WHERE CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CustTemp
       WHERE CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT CustTemp USE-INDEX TemplName
       WHERE CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CustTemp
       WHERE CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV CustTemp USE-INDEX TemplName
       WHERE CustTemp.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CustTemp.Brand 
       CustTemp.TemplNum 
       CustTemp.TemplName
       CustTemp.CustNum
       Customer.CustName WHEN AVAIL Customer

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND Customer WHERE Customer.CustNum = CustTemp.CustNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          CustTemp.TemplName
          CustTemp.CustNum


      WITH FRAME lis
      EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "CustNum" THEN DO:
                   FIND Customer WHERE 
                        Customer.Brand = lcBrand AND 
                        Customer.CustNum =
                   INPUT FRAME lis CustTemp.CustNum NO-LOCK NO-ERROR.
                   IF NOT AVAIL Customer THEN DO:
                      BELL.
                      MESSAGE "Unknown customer !".
                      NEXT.
                   END.    

                END.
             END.
             APPLY LASTKEY.
          END. /* EDITING */

      LEAVE.
   END.
END PROCEDURE.

