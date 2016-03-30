/* ----------------------------------------------------------------------
  MODULE .......: InvRowCounter
  TASK .........: Browse table InvRowCounter
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 15.11.10
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'InvRowCounter'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhInvRowCounter AS HANDLE NO-UNDO.
   lhInvRowCounter = BUFFER InvRowCounter:HANDLE.
   RUN StarEventInitialize(lhInvRowCounter).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhInvRowCounter).
   END.

END.

DEF INPUT PARAMETER iiInvCust  AS INT  NO-UNDO.
DEF INPUT PARAMETER iiMsSeq    AS INT  NO-UNDO.
DEF INPUT PARAMETER iiInvNum   AS INT  NO-UNDO.
DEF INPUT PARAMETER icBillCode AS CHAR NO-UNDO. 

DEF NEW shared VAR siirto AS CHAR.

DEF VAR lcCLI        AS CHAR                   NO-UNDO.
DEF VAR lcBillCode   AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
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

DEF VAR lcExtInvId   AS CHAR  NO-UNDO.
DEF VAR ldaFromDate  AS DATE  NO-UNDO.
DEF VAR ldaToDate    AS DATE  NO-UNDO.
DEF VAR lcCustName   AS CHAR  NO-UNDO.

DEF TEMP-TABLE ttCounter NO-UNDO 
   FIELD CLI AS CHAR
   FIELD BillCode AS CHAR
   FIELD CCN AS INT
   FIELD InvNum AS INT
   FIELD ToDate AS DATE
   FIELD IRCounter AS RECID
   INDEX CLI CLI BillCode CCN ToDate
   INDEX BillCode BillCode CCN CLI ToDate
   INDEX InvNum InvNum CLI BillCode CCN ToDate.

DEF TEMP-TABLE ttMsSeq NO-UNDO
   FIELD MsSeq AS INT
   INDEX MsSeq MsSeq.
   
   
FORM
    lcExtInvId              FORMAT "X(12)" COLUMN-LABEL "Invoice"
    InvRowCounter.CLI       FORMAT "X(10)" 
    InvRowCounter.BillCode  
    InvRowCounter.CCN       
    InvRowCounter.ToDate
    InvRowCounter.Quantity  FORMAT "->>>>>>>9"
    InvRowCounter.Amount
WITH ROW FrmRow CENTERED OVERLAY FrmDown DOWN COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " INVOICE ROW COUNTERS " FRAME sel.

FORM
    InvRowCounter.InvCust    COLON 20
       lcCustName FORMAT "X(35)" NO-LABEL SKIP
    InvRowCounter.MsSeq      COLON 20
       InvRowCounter.InvSeq    COLON 50
    InvRowCounter.CLI        COLON 20 
       lcExtInvID              COLON 50
          FORMAT "X(12)" LABEL "Invoice"
          "(" SPACE(0)
          InvRowCounter.InvNum NO-LABEL  
          SPACE(0) ")"  SKIP
    InvRowCounter.SubInvNum    COLON 50    
       SKIP(1)

    InvRowCounter.BillCode     COLON 20 
       BillItem.BIName NO-LABEL SKIP
    InvRowCounter.CCN          COLON 20    
       CCN.CCNName NO-LABEL SKIP
    InvRowCounter.TariffNum    COLON 20   
    InvRowCounter.DCEvent      COLON 20
       DayCampaign.DCName NO-LABEL FORMAT "X(35)" SKIP
    InvRowCounter.ReportingID COLON 20
       FORMAT "X(35)" 
    InvRowCounter.FromDate    COLON 20
    InvRowCounter.ToDate      COLON 20 
       SKIP(1)
       
    InvRowCounter.Quantity    COLON 20
       InvRowCounter.VatIncl     COLON 50 
    InvRowCounter.RealQty     COLON 20 
       InvRowCounter.Amount      COLON 50
    InvRowCounter.Duration    COLON 20
       InvRowCounter.RefPrice COLON 50
    InvRowCounter.DataAmt     COLON 20 
       InvRowCounter.ExtraAmount COLON 50 
   WITH OVERLAY ROW 1 centered
      COLOR VALUE(cfc) TITLE COLOR VALUE(ctc) ac-hdr SIDE-LABELS FRAME lis.

FORM 
    "BillCode:" lcBillCode FORMAT "X(16)"
    HELP "Enter billing item"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Billing Item "
       COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FORM 
    "MSISDN:" lcCLI FORMAT "X(16)"
    HELP "Enter MSISDN"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
       COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FORM 
   ldaFromDate AT 2 
      LABEL "Period"
      FORMAT "99-99-99" 
      HELP "Period of counters"
   "-"
   ldaToDate 
      NO-LABEL 
      FORMAT "99-99-99" 
      HELP "Period of counters"
   WITH OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE " PERIOD " FRAME fPeriod.


IF iiInvCust = 0 AND iiMsSeq = 0 AND iiInvNum = 0 THEN DO:
   MESSAGE "Nothing selected" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

IF iiInvNum = 0 THEN DO:
   RUN pAskPeriod.
   IF RETURN-VALUE BEGINS "UNDO" THEN RETURN.
END.

RUN pInitTempTable.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE ttCounter THEN ASSIGN
   Memory       = recid(ttCounter)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No counters available" VIEW-AS ALERT-BOX.
      RETURN.
   END.

   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND ttCounter WHERE recid(ttCounter) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttCounter THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttCounter).
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
        ufk   = 0
        ufk[1]= 703
        ufk[7]= 0  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW InvRowCounter.BillCode {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvRowCounter.BillCode WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"4,f4,5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.


      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ttCounter WHERE recid(ttCounter) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttCounter THEN
              ASSIGN FIRSTrow = i Memory = recid(ttCounter).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttCounter THEN DO:
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
                rtab[1] = recid(ttCounter)
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
           IF NOT AVAILABLE ttCounter THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttCounter).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttCounter WHERE recid(ttCounter) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttCounter THEN DO:
           Memory = recid(ttCounter).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttCounter THEN Memory = recid(ttCounter).
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
           FIND ttCounter WHERE recid(ttCounter) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       SET lcBillCode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcBillCode > "" THEN DO:
          FIND FIRST ttCounter USE-INDEX BillCode WHERE 
                     ttCounter.BillCode >= lcBillCode
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE ttCounter THEN NEXT BROWSE.

          ASSIGN
             order      = 1 
             memory     = RECID(ttCounter) 
             must-print = TRUE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f2.
       SET lcCLI WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       
       IF lcCLI > "" THEN DO:
          FIND FIRST ttCounter USE-INDEX CLI WHERE 
                     ttCounter.CLI >= lcCLI
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE ttCounter THEN NEXT BROWSE.

          ASSIGN
             order      = 2 
             memory     = RECID(ttCounter) 
             must-print = TRUE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvRowCounter).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvRowCounter).

       RUN local-disp-row.
       xrecid = recid(InvRowCounter).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttCounter) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttCounter) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN DO:
       FIND ttCounter WHERE recid(ttCounter) = rtab[frame-line(sel)].
       FIND FIRST InvRowCounter WHERE 
          RECID(InvRowCounter) = ttCounter.IRCounter EXCLUSIVE-LOCK.
    END.  
    ELSE DO:
       FIND ttCounter WHERE recid(ttCounter) = rtab[frame-line(sel)] NO-LOCK.
       FIND FIRST InvRowCounter WHERE 
          RECID(InvRowCounter) = ttCounter.IRCounter NO-LOCK.
    END.
    
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN DO:
      FIND FIRST ttCounter NO-LOCK NO-ERROR.
      IF AVAILABLE ttCounter THEN 
         FIND FIRST InvRowCounter WHERE 
            RECID(InvRowCounter) = ttCounter.IRCounter 
         NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN DO:
      FIND LAST ttCounter NO-LOCK NO-ERROR.
      IF AVAILABLE ttCounter THEN 
         FIND FIRST InvRowCounter WHERE 
            RECID(InvRowCounter) = ttCounter.IRCounter 
         NO-LOCK NO-ERROR.
   END.      
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN DO:
      FIND NEXT ttCounter NO-LOCK NO-ERROR.
      IF AVAILABLE ttCounter THEN 
         FIND FIRST InvRowCounter WHERE 
            RECID(InvRowCounter) = ttCounter.IRCounter 
         NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN DO:
      FIND PREV ttCounter NO-LOCK NO-ERROR.
      IF AVAILABLE ttCounter THEN 
         FIND FIRST InvRowCounter WHERE 
            RECID(InvRowCounter) = ttCounter.IRCounter 
         NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-disp-row:

       FIND FIRST InvRowCounter WHERE 
          RECID(InvRowCounter) = ttCounter.IRCounter NO-LOCK.
       
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
          lcExtInvID
          InvRowCounter.CLI
          InvRowCounter.BillCode
          InvRowCounter.CCN
          InvRowCounter.ToDate
          InvRowCounter.Quantity
          InvRowCounter.Amount
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
   lcExtInvID = "".
   IF InvRowCounter.InvNum > 0 THEN 
   FOR FIRST Invoice NO-LOCK WHERE 
             Invoice.InvNum = InvRowCounter.InvNum:
      lcExtInvID = Invoice.ExtInvID.
   END.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:
    
   ActionDetails:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      FIND FIRST BillItem WHERE 
                 BillItem.Brand = gcBrand AND
                 BillItem.BillCode = InvRowCounter.BillCode NO-LOCK NO-ERROR.

      FIND FIRST CCN WHERE
                 CCN.Brand = gcBrand AND
                 CCN.CCN = InvRowCounter.CCN NO-LOCK NO-ERROR.
                 
      IF InvRowCounter.DCEvent > "" THEN            
      FIND FIRST DayCampaign WHERE 
           DayCampaign.Brand   = gcBrand AND
           DayCampaign.DCEvent = InvRowCounter.DCEvent NO-LOCK NO-ERROR.
           
      FIND FIRST Customer WHERE Customer.CustNum = InvRowCounter.InvCust
         NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN 
         lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                       BUFFER Customer).
      ELSE lcCustName = "".

      DISP 
         InvRowCounter.InvCust
         lcCustName
         InvRowCounter.CLI
         InvRowCounter.MsSeq
         InvRowCounter.InvSeq
         lcExtInvID
         InvRowCounter.InvNum
         InvRowCounter.SubInvNum
         
         InvRowCounter.BillCode
         BillItem.BIName WHEN AVAILABLE BillItem
         InvRowCounter.CCN
         CCN.CCNName WHEN AVAILABLE CCN 
         InvRowCounter.TariffNum
         InvRowCounter.DCEvent        
         DayCampaign.DCName WHEN InvRowCounter.DCEvent > "" AND
                                 AVAILABLE DayCampaign
         InvRowCounter.ReportingID
         InvRowCounter.FromDate
         InvRowCounter.ToDate

         InvRowCounter.Quantity
         InvRowCounter.RealQty
         InvRowCounter.DataAmt

         InvRowCounter.VatIncl
         InvRowCounter.Amount
         InvRowCounter.RefPrice
         InvRowCounter.ExtraAmount
      WITH FRAME lis.

      ASSIGN 
        ufk    = 0
        ufk[8] = 8
        ehto   = 0.
         
      RUN Syst/ufkey.

      IF toimi = 8 THEN LEAVE ActionDetails.
   END.

END PROCEDURE.

PROCEDURE pInitTempTable:

   EMPTY TEMP-TABLE ttCounter.
   EMPTY TEMP-TABLE ttMsSeq.

   IF iiInvNum > 0 THEN 
   FOR FIRST Invoice NO-LOCK WHERE
             Invoice.InvNum = iiInvNum,
        EACH SubInvoice NO-LOCK WHERE
             SubInvoice.InvNum = iiInvNum,
        EACH InvRowCounter NO-LOCK WHERE
             InvRowCounter.InvCust = Invoice.CustNum AND
             InvRowCounter.InvSeq  = SubInvoice.InvSeq AND
             (IF icBillCode > "" 
              THEN InvRowCounter.BillCode = icBillCode 
              ELSE TRUE) AND
             (IF iiMsSeq > 0 
              THEN InvRowCounter.MsSeq = iiMsSeq
              ELSE TRUE):
      CREATE ttCounter.
      BUFFER-COPY InvRowCounter TO ttCounter.
      ttCounter.IRCounter = RECID(InvRowCounter).
   END.
   
   ELSE DO:
   
      /* get also invoice customer's counters through subscriptions, because 
         there may be a large quantity of counters on customer level */
      IF iiInvCust > 0 THEN DO:
   
         FOR EACH MsOwner NO-LOCK WHERE
                  MsOwner.InvCust = iiInvCust:
            IF CAN-FIND(FIRST ttMsSeq WHERE ttMsSeq.MsSeq = MsOwner.MsSeq) 
            THEN NEXT.
               
            CREATE ttMsSeq.
            ttMsSeq.MsSeq = MsOwner.MsSeq.
         END.
      END.
          
      ELSE IF iiMsSeq > 0 THEN DO:
         CREATE ttMsSeq.
         ttMsSeq.MsSeq = iiMsSeq.
      END.
   
      FOR EACH ttMsSeq,
          EACH InvRowCounter NO-LOCK WHERE
               InvRowCounter.MsSeq = ttMsSeq.MsSeq AND
               InvRowCounter.ToDate >= ldaFromDate AND
               InvRowCounter.FromDate <= ldaToDate AND
               (IF icBillCode > "" 
                THEN InvRowCounter.BillCode = icBillCode 
                ELSE TRUE):
         CREATE ttCounter.
         BUFFER-COPY InvRowCounter TO ttCounter.
         ttCounter.IRCounter = RECID(InvRowCounter).
      END.
   END.
   
END PROCEDURE.

PROCEDURE pAskPeriod:

   ehto = 9.
   RUN Syst/ufkey.p.

   ASSIGN 
      ldaFromDate = TODAY - 90
      ldaFromDate = DATE(MONTH(ldaFromDate),1,YEAR(ldaFromDate))
      ldaToDate   = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.

   REPEAT WITH FRAME fPeriod ON ENDKEY UNDO, RETRY:
   
      IF RETRY THEN RETURN "UNDO".
    
      PAUSE 0.
      UPDATE ldaFromDate ldaToDate WITH FRAME fPeriod.
      LEAVE.
   END.
   
   HIDE FRAME fPeriod NO-PAUSE.

   RETURN "".
   
END PROCEDURE.

FINALLY:

   HIDE FRAME sel NO-PAUSE.
   si-recid = xrecid.

   ehto = 4.
   RUN Syst/ufkey.

   fCleanEventObjects().
END.


