/* ----------------------------------------------------------------------
  MODULE .......: MSISDNC
  TASK .........: Browse MSISDN Nos. of a Range
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 22-06-99
  CHANGED ......: 24.08.99 pt MSISDN.i
                  14.10.02 jr Removed BillLevel & BillTarget
                  06.11.02 jr Eventlog
                  10.10.03 jp Brand
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msisdn.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER ra-recid AS re NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR CLI  LIKE MSISDN.CLI  NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
DEF VAR CLIFrom       AS c                      NO-UNDO.
DEF VAR CLITo       AS c                      NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMSISDN AS HANDLE NO-UNDO.
   lhMSISDN = BUFFER MSISDN:HANDLE.
   RUN StarEventInitialize(lhMSISDN).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMSISDN).
   END.
END.

form
    MSISDN.CLI      /* COLUMN-LABEL FORMAT */
    MSStat.StatusName    format "x(12)" column-label "Status"
    MSClass.McName   format "x(8)"  column-label "Class"

WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) " MSISDN Nos in Range " + CLIFrom + " - " + CLITo
    + " "
    FRAME sel.

form
   MSISDN.CLI     /* LABEL FORMAT */

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.


form
    MSISDN.CustNum    label "Customer ..." Customer.CustName NO-LABEL AT 25 SKIP
    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(Syst.Var:cfc)
    title COLOR VALUE(Syst.Var:ctc) " Customer Data of MSISDN " + MSISDN.CLI + " "
    side-labels 
    FRAME cust.

form /* seek MSISDN number  BY  CLI */
    m_pref space(0)
    CLI format "x(9)"
    HELP "Enter MSISDN number"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND MSISDN No. "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

FIND MSRange WHERE recid(MSRange) = ra-recid no-lock.
CLIFrom = MSRange.CLIFrom.  CLITo = MSRange.CLITo.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

orders = "By 1,By 2,By 3, By 4".


FIND FIRST MSISDN WHERE 
           MSISDN.Brand = Syst.Var:gcBrand AND 
           MSISDN.CLI >= CLIFrom  AND 
           MSISDN.CLI <= CLITo    AND 
           MSISDN.CustNum = MSRange.Custnum 
NO-LOCK NO-ERROR.
IF AVAILABLE MSISDN THEN ASSIGN
   Memory       = recid(MSISDN)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:

   message "No MSISDN Nos Belong into this Range"
   VIEW-AS ALERT-BOX error.
   RETURN.
END. 

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a MSISDN  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR MSISDN.CLI
           VALIDATE
              (MSISDN.CLI NOT ENTERED OR
              NOT CAN-FIND(MSISDN using  MSISDN.CLI),
              "MSISDN number " + string(INPUT MSISDN.CLI) +
              " already exists !").
           IF INPUT FRAME lis MSISDN.CLI NOT ENTERED THEN 
           LEAVE add-row.
           CREATE MSISDN.
           ASSIGN
           MSISDN.CLI = INPUT FRAME lis MSISDN.CLI.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMSISDN).
           ASSIGN
           Memory = recid(MSISDN)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST MSISDN WHERE 
                 MSISDN.Brand = Syst.Var:gcBrand AND
                 MSISDN.CLI >= CLIFrom  AND 
                 MSISDN.CLI <= CLITo    AND 
                 MSISDN.CustNum = MSRange.Custnum 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MSISDN THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MSISDN WHERE recid(MSISDN) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MSISDN THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MSISDN).
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
        Syst.Var:ufk[1]= 209  Syst.Var:ufk[2]= 0 Syst.Var:ufk[3]= 238 Syst.Var:ufk[4]= 788
        Syst.Var:ufk[5]= 0  Syst.Var:ufk[6]= 0 Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MSISDN.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) MSISDN.CLI WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MSISDN.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) MSISDN.CLI WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = keylabel(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND MSISDN WHERE recid(MSISDN) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MSISDN THEN
              ASSIGN FIRSTrow = i Memory = recid(MSISDN).
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

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE MSISDN THEN DO:
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
                rtab[1] = recid(MSISDN)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE MSISDN THEN DO:
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
              rtab[FRAME-DOWN] = recid(MSISDN).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MSISDN WHERE recid(MSISDN) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MSISDN THEN DO:
           Memory = recid(MSISDN).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MSISDN THEN Memory = recid(MSISDN).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND MSISDN WHERE recid(MSISDN) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISP m_pref WITH FRAME f1.
       SET CLI WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF CLI ENTERED THEN DO:
          FIND FIRST MSISDN WHERE 
                     MSISDN.CLI >= m_pref + CLI AND
                     MSISDN.CLI >= CLIFrom         AND 
                     MSISDN.CLI <= CLITo  AND MSISDN.CustNum = MSRange.Custnum 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE MSISDN THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some MSISDN/mi-no was found */
          ASSIGN order = 1 Memory = recid(MSISDN) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(Syst.Var:nap,"4,f4") > 0 THEN 
CUST:     
     REPEAT: /* show customer data */
        RUN local-find-this (FALSE).
        IF MSISDN.CustNum = 0 THEN DO:
           MESSAGE 
           "This MSISDN " MSISDN.CLI SKIP
           "is not attached to any customer !"
           VIEW-AS ALERT-BOX error.
           LEAVE.
        END.
        PAUSE 0.
        CLEAR FRAME cust no-pause.
CU-DATA:
        repeat WITH FRAME cust:

           FIND Customer of MSISDN no-lock no-error.

           DISP
           MSISDN.CustNum  Customer.CustName when AVAIL Customer
           WITH FRAME cust.
CU-ACTION:
           repeat WITH FRAME cust:
              ASSIGN Syst.Var:ufk = 0 Syst.Var:ufk[8] = 8 Syst.Var:ehto =  0.
              RUN Syst/ufkey.p.
              case Syst.Var:toimi:
                 WHEN 8 THEN DO:
                    HIDE FRAME cust no-pause.
                    LEAVE cu-data.
                 END.   
              END.
           END.     
        END.   
        ufkey = TRUE. 
        LEAVE.
     END.      



     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MSISDN) must-print = TRUE.
       NEXT LOOP.
     END.


    ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MSISDN) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND MSISDN WHERE recid(MSISDN) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MSISDN WHERE recid(MSISDN) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST MSISDN
       WHERE MSISDN.BRAnd = Syst.Var:gcBrand AND MSISDN.CLI >= CLIFrom AND MSISDN.CLI <= CLITo  AND MSISDN.CustNum ~ = MSRange.Custnum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST MSISDN 
       WHERE MSISDN.BRAND = Syst.Var:gcBrand AND MSISDN.CLI >= CLIFrom AND MSISDN.CLI <= CLITo  AND MSISDN.CustNum = MSRange.Custnum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT MSISDN
       WHERE MSISDN.BRAND = Syst.Var:gcBrand AND MSISDN.CLI >= CLIFrom AND MSISDN.CLI <= CLITo  AND MSISDN.CustNum = MSRange.Custnum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV MSISDN
       WHERE MSISDN.BRAND = Syst.Var:gcBrand AND MSISDN.CLI >= CLIFrom AND MSISDN.CLI <= CLITo  AND MSISDN.CustNum = MSRange.Custnum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       MSISDN.CLI
       MSStat.StatusName when AVAIL MSStat
       MSClass.McName when AVAIL MSStat
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND Customer where Customer.CustNum = MSISDN.CustNum no-lock no-error.
   FIND MSStat of MSISDN no-lock no-error.
   FIND MSClass where 
        MSClass.Brand = Syst.Var:gcBrand AND
        MSClass.McCode = MSISDN.McCode no-lock no-error.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          MSISDN.CLI


      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.

