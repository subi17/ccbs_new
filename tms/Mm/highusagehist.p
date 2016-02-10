/* ----------------------------------------------------------------------
  MODULE .......: HighUsage
  TASK .........: UPDATEs table HighUsage
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 21.05.02/tk Event logging added
                  28.02.03 tk tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Func/timestamp.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhHighUsage AS HANDLE NO-UNDO.
   lhHighUsage = BUFFER HighUsage:HANDLE.
   RUN StarEventInitialize(lhHighUsage).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhHighUsage).
   END.

END.

DEF INPUT PARAMETER   cli AS C NO-UNDO.
DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR HighUsage      LIKE HighUsage.InvSeq        NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 3.
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

DEF VAR liAmountOfInvoice  AS INT               NO-UNDO.
DEF VAR ldeInvoiceAverage AS DEC               NO-UNDO.
DEF VAR llOpenInvoice     AS LOG               NO-UNDO FORMAT "X/".
DEF VAR llClaimPerm       AS LOG               NO-UNDO FORMAT "X/".
DEF VAR username          AS CHAR              NO-UNDO FORMAT "X(20)" .
DEF VAR Statusname        AS CHAR              NO-UNDO FORMAT "X(20)" .

DEF VAR lcChanged         AS CHAR              NO-UNDO FORMAT "X(24)" .
DEF VAR lcCreated         AS CHAR              NO-UNDO FORMAT "X(24)" .
DEF VAR liPeriod          AS INT               NO-UNDO FORMAT "999999".
DEF VAR lcperiod          AS CHAR              NO-UNDO FORMAT "X(30)" .
DEF VAR lihakuperiod      AS INT               NO-UNDO FORMAT "999999".

FORM
    Invseq.CustNum     /* COLUMN-LABEL FORMAT */
    liPeriod              COLUMN-LABEL "Period"  
    HighUsage.CLI     /* COLUMN-LABEL FORMAT */
    HighUsage.Amount      COLUMN-LABEL "Amount" FORMAT "ZZZZ9"
    HighUsage.HiUsageStatus COLUMN-LABEL "S"
    lcchanged               COLUMN-LABEL "Last change" 

WITH ROW FrmRow width 70 OVERLAY FrmDown  DOWN CENTERED
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  HIGH SPENDER HISTORY   "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    "Customer ..:" InvSeq.CustNum   Customer.CustName                      SKIP
    "MSISDN no..:" HighUsage.CLI    Username                               SKIP 
    "Period ....:" lcperiod                                                SKIP
    "Amount.....:" HighUsage.Amount FORMAT "zzzz9.999"                     SKIP     "Qty........:" HighUsage.Qty    FORMAT "zzzz9"
    "           Dur(sec):"  HighUsage.duration                             SKIP
    "grow %.....:" HighUsage.Date%                                         SKIP
    "grow amount:" HighUsage.DateGrow                                      SKIP
    "Amt of Inv.:" LiAmountOfInvoice                                       SKIP
    "Avg.of Inv.:" ldeInvoiceAverage                                       SKIP
    "Open inv...:" llOpenInvoice   FORMAT "Yes/No"                         SKIP
    "Claim Perm.:" llClaimPerm     FORMAT "Yes/No"                         SKIP
    "Category ..:" HighUsage.Category                                      SKIP
    "Trigger ...:" HighUsage.Launch                                        SKIP
    "Status ....:" HighUsage.HiUsageStatus StatusName                      SKIP
    "Created ...:" HighUsage.crstamp     lcCreated                         SKIP
    "Changed ...:" HighUsage.chstamp     lcChanged                         
    

WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABEL
    FRAME lis.


form /* seek  HighUsage */
    "Customer" highUsage
    HELP "Enter Customer Number "
    "Period" lihakuperiod 
    HELP "Enter Period" 
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUSTOMER "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  CLI */
    CLI
    HELP "Enter MSISDN Number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.



cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "   By Customer ,   By MSISDN    ,    By amount  , By 4".


FIND FIRST HighUsage
WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
IF AVAILABLE HighUsage THEN ASSIGN
   Memory       = recid(HighUsage)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No billing items available !" VIEW-AS ALERT-BOX.
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
PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND HighUsage WHERE recid(HighUsage) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE HighUsage THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(HighUsage).
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
        ufk[1]= 0   ufk[2]= 0 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 0
        ufk[7]= 0
        ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW InvSeq.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvSeq.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW HighUsage.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) HighUsage.CLI WITH FRAME sel.
      END.

      IF order = 3 THEN DO:
        CHOOSE ROW HighUsage.amount ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) HighUsage.Amount WITH FRAME sel.
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
        FIND HighUsage WHERE recid(HighUsage) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE HighUsage THEN
              ASSIGN FIRSTrow = i Memory = recid(HighUsage).
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
           IF NOT AVAILABLE HighUsage THEN DO:
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
                rtab[1] = recid(HighUsage)
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
           IF NOT AVAILABLE HighUsage THEN DO:
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
              rtab[FRAME-DOWN] = recid(HighUsage).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND HighUsage WHERE recid(HighUsage) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE HighUsage THEN DO:
           Memory = recid(HighUsage).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE HighUsage THEN Memory = recid(HighUsage).
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
           FIND HighUsage WHERE recid(HighUsage) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       /* mobsub detailed information */

       RUN local-find-this(FALSE).

       RUN Mm/highusagemore.p(INPUT HighUsage.invseq, HighUsage.cli).

       ufkey = TRUE. 
       NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhHighUsage).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY Highusage.cli   .

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhHighUsage).

       RUN local-disp-row.
       xrecid = recid(HighUsage).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(HighUsage) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(HighUsage) must-print = TRUE.
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
      FIND HighUsage WHERE recid(HighUsage) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND HighUsage WHERE recid(HighUsage) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST HighUsage
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST HighUsage USE-INDEX CLI
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST HighUsage USE-INDEX Amount
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST HighUsage
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST HighUsage USE-INDEX CLI
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST HighUsage USE-INDEX Amount
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT HighUsage
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT HighUsage USE-INDEX CLI
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT HighUsage USE-INDEX Amount
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV HighUsage
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV HighUsage USE-INDEX CLI
       WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV HighUsage USE-INDEX Amount
        WHERE HighUsage.cli = CLI NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       Invseq.CustNum
       HighUsage.CLI
       liperiod
       HighUsage.Amount 
       HighUsage.HiUsageStatus
       substring(lcchanged,2,19) @ lcchanged
       
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   
   FIND FIRST Invseq WHERE 
              Invseq.Invseq = HighUsage.Invseq NO-LOCK NO-ERROR.

   ASSIGN
      liAmountOfInvoice =  0
      llOpenInvoice    = FALSE
      llClaimPerm      = FALSE.
   FOR EACH invoice WHERE 
            Invoice.Brand    = gcBrand AND 
            Invoice.Custnum  = Invseq.CustNum
            NO-LOCK.
      ASSIGN 
         liAmountOfInvoice = liAmountOfInvoice + 1.
      IF Invoice.PaymState = 0 THEN llOpenInvoice    = TRUE.
      IF Invoice.ClaimPerm = TRUE THEN llClaimPerm   = TRUE.
   END.

   ASSIGN
      i                 = 0
      ldeInvoiceAverage = 0 .

   FOR EACH Invoice NO-LOCK WHERE 
            Invoice.Brand    = gcBrand AND 
            Invoice.Custnum  = Invseq.CustNum AND
            Invoice.InvDate >= today - 90,
      FIRST SubInvoice OF Invoice NO-LOCK WHERE
            SubInvoice.CLI = Highusage.cli:

      ASSIGN
         i = i + 1
         ldeInvoiceAverage = ldeInvoiceAverage + SubInvoice.InvAmt.
   END.        

   ldeInvoiceAverage = ldeInvoiceAverage / i.

   if ldeinvoiceaverage = ? THEN ldeinvoiceaverage = 0.
   
   username = "".
   
   FIND FIRST msowner where 
              msowner.cli = HighUsage.cli no-lock no-error.
   IF avail msowner then 
   FIND FIRST Customer WHERE 
              Customer.Custnum = Msowner.CustNum NO-LOCK NO-ERROR.
              
   IF avail Customer THEN 
   Username = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER Customer).
   
   lcCreated = "(" + fTS2HMS(HighUsage.crstamp) + ")".     
   lcChanged = "(" + fTS2HMS(HighUsage.chstamp) + ")" .               

   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "HighUsage"      AND
              TMSCodes.FieldName = "HiUsageStatus"  AND 
              TMSCodes.CodeGroup = "HighUsage"      AND
              TMSCodes.CodeValue = STRING(HighUsage.HiUsageStatus)
   NO-LOCK NO-ERROR.
   IF AVAIL tmscodes THEN Statusname = TMSCodes.CodeName.
   ELSE                   STATUSNAME = "".

   ASSIGN
      lcperiod = STRING(Invseq.FromDate) + " - " + STRING(Invseq.Todate)
      liperiod = YEAR(Invseq.todate) * 100 + MONTH(Invseq.todate).

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      InvSeq.CustNum   Customer.CustName  WHEN AVAIL Customer                     
      HighUsage.CLI    Username                               
      HighUsage.Amount                                        
      HighUsage.Qty                                           
      HighUsage.duration                                      
      HighUsage.Date%                                         
      HighUsage.DateGrow                                      
      LiAmountOfInvoice                                       
      ldeInvoiceAverage                                       
      llOpenInvoice                                           
      llClaimPerm                                             
      lcPeriod 
      HighUsage.Category                                      
      HighUsage.Launch                                        
      HighUsage.HiUsageStatus StatusName                      
      HighUsage.crstamp     lcCreated                         
      HighUsage.chstamp     lcChanged                         
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
         MESSAGE "PRESS ENTER" .
         PAUSE NO-MESSAGE.                 
         
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

