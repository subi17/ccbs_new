/* ----------------------------------------------------------------------
  MODULE .......: TMQueue
  TASK .........: 
  APPLICATION ..: 
  AUTHOR .......: Nostradamus
  CREATED ......: 23.04.2008
  CHANGED ......: 
  Version ......: SCRUNKO4 (10.06.99)
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msisdn.i}
{Func/func.i}
   
DEF /* NEW */ shared VAR siirto AS CHAR.


DEFINE VARIABLE tthCDR     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lcHostName AS CHARACTER NO-UNDO.
DEFINE VARIABLE llPrepaid  AS LOG       NO-UNDO.


DEF VAR DateSt   LIKE Mobcdr.DateSt               NO-UNDO.
DEF VAR timest   LIKE Mobcdr.timest               NO-UNDO.
DEF VAR timest1  LIKE Mobcdr.TimeStart            NO-UNDO.
DEF VAR CustNum  LIKE Mobcdr.CustNum              NO-UNDO.
DEF VAR CLI      LIKE Mobcdr.CLI                  NO-UNDO.
DEF VAR gsmbnr   LIKE Mobcdr.Gsmbnr               NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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
DEF VAR qmark        AS DA                     NO-UNDO  INIT ?.
DEF VAR stime        AS C                      NO-UNDO.
DEF VAR UserName     AS C                      NO-UNDO.
DEF VAR def-ccode    AS C                      NO-UNDO.
DEF VAR Billed       AS LO                     NO-UNDO.
DEF VAR SL_prefix    AS C                      NO-UNDO.
DEF VAR lii          AS INT                    NO-UNDO.

{Func/tmsparam.i SL_prefix      return}.  SL_prefix = TMSParam.CharVal.
{Func/tmsparam.i DefCCode       return}.  def-ccode = TMSParam.CharVal.


DEFINE TEMP-TABLE ttCall LIKE Mobcdr.

DEF BUFFER bTMQueue FOR TMQueue.
            
form
    TMQueue.DateSt
    TMQueue.MSSeq        Column-label "SubSeq" 
    TMQueue.BillCode     COLUMN-LABEL "Pr"          FORMAT "x(4)"
    TMQueue.CustNum      COLUMN-LABEL "A-Cust"      FORMAT "zzzzzzzz9"
    UserName              COLUMN-LABEL "Cust. Name"  FORMAT "x(24)"
    TMQueue.qty         COLUMN-LABEL "Q"            FORMAT "-9"
    TMQueue.Amount


WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " TM Queue "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
   TMQueue.EventID
   TMQueue.MSSeq
   TMQueue.CustNum
   TMQueue.Datest
   TMQueue.BillCode
   TMQueue.RateCCN
   TMQueue.Bdest
   TMQueue.SpoCMT
   TMQueue.Qty
   TMQueue.Amount
   TMQueue.BillDur
   TMQueue.DataIn
   TMQueue.DataOut
                    
   WITH  OVERLAY ROW 4 centered
   COLOR VALUE(cfc)
   TITLE COLOR VALUE(ctc) ac-hdr
   SIDE-LABELS  1 columns    FRAME lis.


FORM
"Queue:" lii no-label

WITH  OVERLAY ROW 10 centered
FRAME calc.
            

form /* seek Mobile Call  BY  DateSt */
    DateSt timest
    HELP "Enter CallDate and time"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date AND TIME "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Mobile Call  BY CustNum */
    CustNum
    HELP "Enter A-Sub Customer No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND A-CUST "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek Mobile Call  BY A-sub. */

    CLI FORMAT "x(12)"
    HELP "Enter calling MSISDN No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

form /* seek Mobile Call  BY A-sub. */
    gsmbnr FORMAT "x(11)"
    HELP "Enter called b-No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND B-NUMBER "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f4.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = 
"By Call Date, By Product, By A-Customer, BY MSISDN No., By CallType ".

FIND FIRST TMQueue NO-LOCK NO-ERROR.

llPrePaid = FALSE.

FIND FIRST TMQueue NO-LOCK NO-ERROR.

IF AVAILABLE TMQueue THEN ASSIGN
   Memory       = recid(TMQueue)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE 
   "There are NO Billable Calls"  SKIP
   "in the call database"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a TMQueue  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR TMQueue.DateSt
           VALIDATE
              (TMQueue.DateSt NOT ENTERED OR
              NOT CAN-FIND(TMQueue using  TMQueue.DateSt),
              "Mobile Call " + string(INPUT TMQueue.DateSt) +
              " already exists !").
           IF INPUT FRAME lis TMQueue.DateSt NOT ENTERED THEN 
           LEAVE add-row.
           CREATE TMQueue.
           ASSIGN
           TMQueue.DateSt = INPUT FRAME lis TMQueue.DateSt.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           Memory = recid(TMQueue)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST TMQueue NO-LOCK NO-ERROR.
           
      IF NOT AVAILABLE TMQueue THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND TMQueue WHERE recid(TMQueue) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TMQueue THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TMQueue).
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
        ufk[1]= 0 ufk[2]= 0 ufk[3]= 2630 ufk[4]= 265
        ufk[5]= 2421 ufk[6]= 0   ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TMQueue.DateSt ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMQueue.DateSt WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW TMQueue.BillCode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMQueue.BillCode WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW TMQueue.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMQueue.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW TMQueue.MSSeq ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMQueue.MSSeq WITH FRAME sel.
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
        FIND TMQueue WHERE recid(TMQueue) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TMQueue THEN
              ASSIGN FIRSTrow = i Memory = recid(TMQueue).
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
           IF NOT AVAILABLE TMQueue THEN DO:
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
                rtab[1] = recid(TMQueue)
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
           IF NOT AVAILABLE TMQueue THEN DO:
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
              rtab[FRAME-DOWN] = recid(TMQueue).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TMQueue WHERE recid(TMQueue) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TMQueue THEN DO:
           Memory = recid(TMQueue).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TMQueue THEN Memory = recid(TMQueue).
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
           FIND TMQueue WHERE recid(TMQueue) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       SET CustNum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CustNum ENTERED THEN DO:
          FIND FIRST TMQueue  NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TMQueue THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some TMQueue/CustNum was found */
          ASSIGN order = 2 Memory = recid(TMQueue) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     
     ELSE IF  LOOKUP(nap,"3,f3") > 0 THEN DO:

        FOR EACH bTMQueue NO-LOCK.
           lii = lii + 1.
           
           IF lii mod 100 = 0 THEN DISP lii WITH FRAME calc.
           PAUSE 0.
        END.

        MESSAGE "-PRESS ENTER TO CONTINUE-". PAUSE NO-MESSAGE.
     
     END.
     
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* VIEW */

        PAUSE 0.
        DISP
        TMQueue.EventID 
        TMQueue.MSSeq 
        TMQueue.CustNum
        TMQueue.Datest
        TMQueue.BillCode
        TMQueue.RateCCN
        TMQueue.Bdest
        TMQueue.SpoCMT
        TMQueue.Qty
        TMQueue.Amount
        TMQueue.BillDur
        TMQueue.DataIn
        TMQueue.DataOut
        WITH FRAME lis.
        
        MESSAGE 
        " -PRESS ENTER TO CONTINUE-". PAUSE NO-MESSAGE.
      

        ufkey = TRUE.
        NEXT loop.
     END.
     ELSE IF LOOKUP(nap,"enter,return,5,f5") > 0 THEN DO:  /* VIEW CDR*/

        FIND FIRST TMQueue WHERE
             recid(TMQueue) = rtab[FRAME-LINE] NO-LOCK NO-ERROR.

        FIND FIRST Mobsub WHERE mobsub.msseq = TMQueue.MSSeq NO-LOCK NO-ERROR.

        IF AVAIL mobsub THEN 
        FIND FIRST Mobcdr WHERE
                   Mobcdr.CLI    = Mobsub.CLI    AND 
                   Mobcdr.datest = TMQueue.Datest AND 
                   Mobcdr.dtlseq = TMQueue.EventId NO-LOCK NO-ERROR.

        IF AVAIL Mobcdr THEN DO:
           
           CREATE ttCall.
           BUFFER-COPY mobcdr TO ttCall.
           
           RUN Mm/viewmbd(INPUT TABLE ttCall,
                       ttCall.datest,
                       ttCall.Timest,
                       ttCall.cli,
                       ttCall.DtlSeq).

          EMPTY TEMP-TABLE ttCall.
        END.
        
        ufkey = TRUE.
        NEXT loop.
     END.    

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY TMQueue.DateSt.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(TMQueue).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TMQueue) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TMQueue) must-print = TRUE.
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
      FIND TMQueue WHERE recid(TMQueue) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TMQueue WHERE recid(TMQueue) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF ORder = 1 THEN FIND FIRST TMQueue NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST TMQueue NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF Order = 1 THEN  FIND NEXT TMQueue NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF ORder = 1  THEN FIND PREV TMQueue NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       TMQueue.DateSt
       TMQueue.CustNum
       UserName
       TMQueue.Qty
       TMQueue.Amount
       TMQueue.MSSeq  
       TMQueue.BillCode
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND MobSub WHERE MobSub.MsSeq = TMQueue.MsSeq NO-LOCK NO-ERROR.
       FIND Customer WHERE Customer.CustNum = TMQueue.CustNum NO-LOCK NO-ERROR.

       IF Avail Customer then username = DYNAMIC-FUNCTION("fDispCustName" IN                        ghFunc1,  BUFFER Customer) .
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          TMQueue.CustNum


      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.

