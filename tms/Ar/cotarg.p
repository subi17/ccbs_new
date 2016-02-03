/* ----------------------------------------------------------------------
  MODULE .......: CoTarg
  TASK .........: UPDATEs table CoTarg
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 31.12.02
  CHANGED ......: 19.09.03/aam brand
                  13.10.03/aam tokens
                  11.11.03/aam SubsQty
                  30.08.04/aam percents and fixed amounts moved to cobasis
                  15.09.04/aam use SubsQty for MsSeq
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}

{Func/date.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'cotarg'}
DEF INPUT PARAMETER iiKey AS INT NO-UNDO. 
DEF INPUT PARAMETER icType   AS CHAR NO-UNDO. 

DEF NEW shared VAR siirto AS CHAR.

DEF VAR lcTarg       LIKE CoTarg.TargType      NO-UNDO.
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
DEF VAR lcTitle      AS CHAR                   NO-UNDO. 
DEF VAR lcTargName   AS CHAR                   NO-UNDO. 

DEFINE VARIABLE lcCommStatus   AS CHARACTER NO-UNDO FORMAT "x(10)".
DEFINE VARIABLE lcStatusReason AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCreated      AS CHARACTER NO-UNDO FORMAT "x(20)". 
DEFINE VARIABLE lcHandled      AS CHARACTER NO-UNDO FORMAT "x(20)".

form
    CoTarg.CoTarg COLUMN-LABEL "Target"
    lcTargName    COLUMN-LABEL "Description" FORMAT "X(19)"
    lcCreated FORMAT "x(10)" column-label "Created"
    lcHandled FORMAT "x(10)" column-label "Handled" 
    CoTarg.CommStatus
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
    lcTitle
    FRAME sel.

FORM
    "Target ...........:" COTarg.CoTarg 
    lcTargName NO-LABEL FORMAT "X(30)" SKIP
    "Created ..........:" lcCreated SKIP 
    "Handled ..........:" lcHandled SKIP
    "Commission Status.:" COTarg.CommStatus lcCommStatus FORMAT "x(30)" SKIP
    "Status Reason ....:" COTarg.StatusReason lcStatusReason FORMAT "x(40)" SKIP
    "Amount ...........:" CORule.CommAmount SKIP
    "Order ID .........:" COTarg.OrderID SKIP
    "Commission ID ....:" COTarg.CoTargID SKIP
    "Commission Rule ID:" COTarg.CORuleId SKIP
WITH  OVERLAY ROW 5 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) "COMMISSION INFO" 
    NO-LABELS 
    FRAME lis.

form /* seek  target */
    lcTarg
    HELP "Enter target type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND target type "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

IF icType = "rule" THEN DO:
FIND CoRule WHERE 
     CoRule.Brand = gcBrand AND
     CoRule.CoRuleId = iiKey NO-LOCK.
ASSIGN 
   lcTitle = " COMM. TARGETS FOR: " +
                 STRING(CoRule.CoRuleID) + "/" +
                 STRING(CoRule.RuleDesc) + " ".
END.

ELSE IF icType = "mobsub" THEN
   lcTitle = "COMMISSIONS FOR SUBSCRIPTION: " + STRING(iiKey).

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE CoTarg THEN ASSIGN
   Memory       = recid(CoTarg)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.
   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CoTarg WHERE recid(CoTarg) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CoTarg THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CoTarg).
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
        ufk = 0.
        ASSIGN
        ufk[2]= 0  ufk[3]= 0  
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CoTarg.COTarg ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CoTarg.COTarg WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW CoTarg.CoTarg ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CoTarg.CoTarg WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW CoTarg.CommStatus ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CoTarg.CommStatus WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
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
        FIND CoTarg WHERE recid(CoTarg) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CoTarg THEN
              ASSIGN FIRSTrow = i Memory = recid(CoTarg).
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
           IF NOT AVAILABLE CoTarg THEN DO:
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
                rtab[1] = recid(CoTarg)
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
           IF NOT AVAILABLE CoTarg THEN DO:
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
              rtab[FRAME-DOWN] = recid(CoTarg).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CoTarg WHERE recid(CoTarg) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CoTarg THEN DO:
           Memory = recid(CoTarg).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CoTarg THEN Memory = recid(CoTarg).
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
           FIND CoTarg WHERE recid(CoTarg) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  /* SHARING */
       {Syst/uright2.i}
       RUN local-find-this (FALSE).
       IF AVAILABLE CoTarg 
       THEN RUN coshare.p (CoTarg.CoTargID). 
       ufkey = true. 
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 AND lcRight = "RW"
     THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).
      
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       /*DISPLAY CoTarg.TargType.*/

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(CoTarg).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CoTarg) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CoTarg) must-print = TRUE.
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
      FIND CoTarg WHERE recid(CoTarg) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CoTarg WHERE recid(CoTarg) = rtab[frame-line(sel)] 
    NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       
       IF order = 1 THEN 
       CASE icType:
          WHEN "Rule" THEN
              FIND FIRST CoTarg
              WHERE CoTarg.Brand = gcBrand AND CoTarg.CoRuleId = iiKey
              NO-LOCK NO-ERROR.
          WHEN "Mobsub" THEN 
             FIND FIRST CoTarg
              WHERE CoTarg.Brand = gcBrand AND 
                    CoTarg.TargType = "M" AND
                    CoTarg.COTarg = STRING(iiKey)
              NO-LOCK NO-ERROR.
       END.
END PROCEDURE.

PROCEDURE local-find-LAST:
       
       IF order = 1 THEN 
       CASE icType:
          WHEN "Rule" THEN
              FIND LAST CoTarg
              WHERE CoTarg.Brand = gcBrand AND CoTarg.CoRuleId = iiKey
              NO-LOCK NO-ERROR.
          WHEN "Mobsub" THEN 
             FIND LAST CoTarg
              WHERE CoTarg.Brand = gcBrand AND 
                    CoTarg.TargType = "M" AND
                    CoTarg.COTarg = STRING(iiKey)
              NO-LOCK NO-ERROR.
       END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       
       IF order = 1 THEN  
       CASE icType:
          WHEN "Rule" THEN
              FIND NEXT CoTarg
              WHERE CoTarg.Brand = gcBrand AND CoTarg.CoRuleId = iiKey
              NO-LOCK NO-ERROR.
          WHEN "Mobsub" THEN 
             FIND NEXT CoTarg
              WHERE CoTarg.Brand = gcBrand AND 
                    CoTarg.TargType = "M" AND
                    CoTarg.COTarg = STRING(iiKey)
              NO-LOCK NO-ERROR.
       END.
END PROCEDURE.

PROCEDURE local-find-PREV:
       
       IF order = 1 THEN  
       CASE icType:
          WHEN "Rule" THEN
              FIND PREV CoTarg
              WHERE CoTarg.Brand = gcBrand AND CoTarg.CoRuleId = iiKey
              NO-LOCK NO-ERROR.
          WHEN "Mobsub" THEN 
             FIND PREV CoTarg
              WHERE CoTarg.Brand = gcBrand AND 
                    CoTarg.TargType = "M" AND
                    CoTarg.COTarg = STRING(iiKey)
              NO-LOCK NO-ERROR.
       END.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CoTarg.CoTarg
       lcTargName 
       CoTarg.CommStatus
       lcHandled
       lcCreated
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   lcTargName = "Unknown". 

   CASE CoTarg.TargType:
   WHEN "C" THEN DO:
      FIND Customer WHERE Customer.CustNum = INTEGER(CoTarg.CoTarg) 
         NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN lcTargName = Customer.CustName.
   END. 
   WHEN "R" THEN DO:
      FIND FIRST Reseller WHERE 
                 Reseller.Brand    = gcBrand AND
                 Reseller.Reseller = CoTarg.CoTarg NO-LOCK NO-ERROR.
      IF AVAILABLE Reseller THEN lcTargName = Reseller.RsName.
   END.
   WHEN "S" THEN DO:
      FIND FIRST Salesman WHERE 
                 Salesman.Brand    = gcBrand AND
                 Salesman.Salesman = CoTarg.CoTarg 
         NO-LOCK NO-ERROR.
      IF AVAILABLE Salesman THEN lcTargName = Salesman.SmName.
   END.
   WHEN "M" THEN DO:
      FIND FIRST MsOwner WHERE
                 MsOwner.MsSeq = INTEGER(CoTarg.CoTarg) NO-LOCK NO-ERROR.
      IF AVAILABLE MsOwner THEN lcTargName = MsOwner.CLI.
   END.
   
   OTHERWISE lcTargName = "". 

   END CASE.       
   
   lcCommStatus = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "CoTarg",
                               "CommStatus",
                               STRING(COTarg.CommStatus)).
   
   lcstatusreason = dynamic-function("ftmscodename" in ghfunc1,
                               "cotarg",
                               "statusreason",
                               string(cotarg.statusreason)).
   
   lcCreated = fTS2HMS(COTarg.CreatedTS).
   lcHandled = fTS2HMS(COTarg.HandledTS).
      
   FIND CORule WHERE
      CORule.Brand = gcBrand AND
      CORule.CORuleID = COTarg.CORuleId NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      
      
      RUN local-find-others.
      
      DISP 
           CoTarg.CoTarg
           lcTargName 
           CoTarg.CommStatus lcCommStatus
           CoTarg.StatusReason lcStatusReason
           lcHandled
           lcCreated
           STRING(COTarg.COTargID) @ COTarg.COTargID
           STRING(COTarg.OrderID) @ COTarg.OrderID
           STRING(COTarg.CORuleId) @ COTarg.CORuleId
           STRING(CORule.CommAmount)  WHEN AVAIL CORule @ CORule.CommAmount

      WITH FRAME lis.
      
      ASSIGN
          ufkey = TRUE
          ufk   = 0  
          ehto  = 1
          ufk[5] = 927
          ufk[8]= 8.
      RUN ufkey.          

      IF toimi = 5 THEN DO:
         RUN memo("0",
                  INPUT "CoTarg",
                  INPUT STRING(CoTarg.COTargId),
                  INPUT "Commission").
         ufkey = true.
         NEXT.
      END.
           
      LEAVE.
   END.
END PROCEDURE.

