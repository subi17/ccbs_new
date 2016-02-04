/* ----------------------------------------------------------------------
  MODULE .......: MSOwner.p
  TASK .........: Browser of mobile subscription owners
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 11-08-99
  CHANGED ......: 06.10.99 pt  Layout, F4
                  07.12.99 jp  MSOwner.IMSI
                  29.12.99 jpo Layout, F4, Added UPDATE MSOwner.TsEnd
                  14.10.02 jr  Removed BillLevel
                  06.11.02 jr  Eventlog
                  03.03.03 tk  tokens
                  04.09.03 jp  Brand 
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'msowner'}

DEF SHARED VAR siirto AS CHAR.

DEF INPUT PARAMETER  CLI LIKE mobsub.CLI NO-UNDO. 
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
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
DEF VAR end-txt      AS C   format "x(20)"     NO-UNDO.
DEF VAR UserName     AS C                      NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMSOwner AS HANDLE NO-UNDO.
   lhMSOwner = BUFFER MSOwner:HANDLE.
   RUN StarEventInitialize(lhMSOwner).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMSOwner).
   END.
END.

form
    MSOwner.CustNum    format "zzzzzzzz9"
    MSOwner.BillTarget
    UserName           FORMAT "x(20)" COLUMN-LABEL "Name of User"
    MSOwner.TsBegin
    MSOwner.TsEnd

WITH OVERLAY ROW FrmRow FrmDown DOWN centered

    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " KNOWN OWNERS OF MOBILE SUBSCRIPTION " + msowner.CLI + " "
    FRAME sel.

form
    "MSISDN Number ...:" MSOwner.CLI                                  skip(1)
    "Customer ........:" MSOwner.CustNum TO 30 Customer.CustName  AT 35    SKIP
    "Mobile user .....:" MSOwner.MsSeq TO 30 Customer.CustName  AT 35    SKIP
    "Invoicing Target :" MSOwner.BillTarget        BillTarg.BillTarget  AT 35 
    SKIP(1)
    "Starting Date ...:" MSOwner.TsBegin                               SKIP
    "Ending Date .....:" MSOwner.TsEnd end-txt                         SKIP
    "IMSI Number .....:" MSOwner.IMSI                                  SKIP
    "Cli.type.........:" MSOwner.Clitype CLIType.CliName 
    "Contract ........:" MSOwner.Contract                              SKIP
    "InportOperator...:" msowner.inportOper                             SKIP



WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    /*1 columns*/
    FRAME lis.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Date,By 2,By 3, By 4".


FIND FIRST MSOwner WHERE 
           MSOwner.CLI = CLI NO-LOCK NO-ERROR.
IF AVAILABLE MSOwner THEN ASSIGN
   Memory       = recid(MSOwner)
   must-print   = TRUE
   must-add     = FALSE.

ELSE DO: 
  MESSAGE
  "This MSISDN Number doesn't have any user !"
  VIEW-AS ALERT-BOX ERROR.
  LEAVE.
END.


LOOP:
REPEAT WITH FRAME sel:
    /* Changed */
    IF order <> pr-order AND MaxOrder = 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 35 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a MSOwner  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR MSOwner.CLI
           VALIDATE
              (MSOwner.CLI NOT ENTERED OR
              NOT CAN-FIND(MSOwner using  MSOwner.CLI),
              "MSOwnerS " + string(INPUT MSOwner.CLI) +
              " already exists !").
           IF INPUT FRAME lis MSOwner.CLI NOT ENTERED THEN 
           LEAVE add-row.
           CREATE MSOwner.
           ASSIGN
           MSOwner.CLI   = INPUT FRAME lis MSOwner.CLI
           MSOWner.Brand = gcBrand .

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMSOwner).
           ASSIGN
           Memory = recid(MSOwner)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST MSOwner
      WHERE MSOwner.CLI = CLI NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MSOwner THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MSOwner WHERE recid(MSOwner) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MSOwner THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MSOwner).
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
        ufk[1]= /*35*/ 0     ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 0  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MSOwner.TsBegin ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MSOwner.TsBegin WITH FRAME sel.
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
        FIND MSOwner WHERE recid(MSOwner) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MSOwner THEN
              ASSIGN FIRSTrow = i Memory = recid(MSOwner).
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
           IF NOT AVAILABLE MSOwner THEN DO:
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
                rtab[1] = recid(MSOwner)
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
           IF NOT AVAILABLE MSOwner THEN DO:
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
              rtab[FRAME-DOWN] = recid(MSOwner).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MSOwner WHERE recid(MSOwner) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MSOwner THEN DO:
           Memory = recid(MSOwner).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MSOwner THEN Memory = recid(MSOwner).
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
           FIND MSOwner WHERE recid(MSOwner) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"F4,4") > 0 THEN DO TRANS:
        RUN local-find-this(TRUE).
        DISPLAY
            MSOwner.CustNum 
            MSOwner.TsBegin 
            MSOwner.TsEnd 
        WITH FRAME SEL.

        IF lcRight = "RW" THEN DO:

           ehto = 9. RUN Syst/ufkey.

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSOwner).
           
           UPDATE 
              MSOwner.CustNum 
              MSOwner.TsBegin 
              MSOwner.TsEnd 
           WITH FRAME SEL.
           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSOwner).

           ufkey = TRUE.
        END.
        ELSE PAUSE.
        RELEASE msowner.
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).
       ASSIGN ac-hdr = " VIEW MSOwner " ufkey = TRUE ehto = 9. /*RUN Syst/ufkey.*/
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY MSOwner.CLI.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSOwner).
       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSOwner).
       RUN local-disp-row.
       xrecid = recid(MSOwner).
       RELEASE msowner.
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MSOwner) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MSOwner) must-print = TRUE.
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
      FIND FIRST MSOwner WHERE recid(MSOwner) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FIRST MSOwner WHERE recid(MSOwner) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST MSOwner
       WHERE MSOwner.CLI = CLI NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST MSOwner
       WHERE MSOwner.CLI = CLI NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT MSOwner
       WHERE MSOwner.CLI = CLI NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV MSOwner
       WHERE MSOwner.CLI = CLI NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       MSOwner.CustNum 
       MSOwner.BillTarget
       UserName          
       MSOwner.TsBegin
       MSOwner.TsEnd


       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND BillTarg WHERE 
        BillTarg.CustNum    = MSOwner.CustNum AND
        BillTarg.BillTarget = MSOwner.BillTarget
   NO-LOCK NO-ERROR.

   FIND MobSub  WHERE 
        MobSub.CLI    = MSOwner.CLI NO-LOCK NO-ERROR.

   FIND Customer WHERE 
        Customer.CustNum   = MSOwner.CustNum NO-LOCK NO-ERROR.

   IF MSOwner.TsEnd = 99999999.99999 THEN DO:
      end-txt = "Present User".
   END.
   ELSE DO:
      end-txt = "Previous User".
   END.
   IF AVAIL MobSub THEN 
      ASSIGN UserName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                          BUFFER Customer).
                                             
   ELSE 
      ASSIGN UserName = "!! REMOVED USER !!".
   FIND CLIType where 
        CliType.Brand   = gcBrand  AND 
        CLIType.Clitype = MSOwner.Clitype NO-LOCK NO-ERROR.  

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP 
      MSOwner.CLI
      MSOwner.CustNum 
      UserName WHEN AVAIL Customer
      MSOwner.msseq
      MSOwner.BillTarget
      BillTarg.BillTarget WHEN AVAIL BillTarg
      MSOwner.TsBegin
      MSOwner.TsEnd
      end-txt
      MSOwner.IMSI
      MSOwner.Clitype
      CLIType.CliName WHEN AVAIL CLIType
      msowner.contract
      msowner.inportoper
      WITH FRAME lis.
      message "Press ENTER !".
      PAUSE no-message.

      UPDATE

      WITH FRAME lis. 

      LEAVE.
   END.
END PROCEDURE.

