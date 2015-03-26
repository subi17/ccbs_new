/* ----------------------------------------------------------------------
  MODULE .......: ServiceLCounter
  TASK .........: Show all/MSSeq ServiceLCounter records
  APPLICATION ..: TMS
  AUTHOR .......: jp
  CREATED ......: 17.06.03
  CHANGED ......: as / 200804 
  VERSION ......: xfera
  ---------------------------------------------------------------------- */

{commali.i}
{date.i}
{tmsconst.i}
{nnpura.i}
 
DEF INPUT PARAM iMSSeq    AS INT  NO-UNDO.
DEF INPUT PARAM iiCustNum AS INT  NO-UNDO.
DEF INPUT PARAM iSLSeq    AS INT  NO-UNDO.
DEF INPUT PARAM icEvent   AS CHAR NO-UNDO.
DEF INPUT PARAM iiPeriod  AS INT  NO-UNDO. 
DEF INPUT PARAM iiMSID  AS INT  NO-UNDO. 

def /* new */ shared var siirto AS char.

DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 1.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcName       AS char                   NO-UNDO.
DEF VAR lceventname  AS CHAR                   NO-UNDO.
DEF VAR usedamt      AS CHAR                   NO-UNDO.
DEF VAR lcInclUnit   AS CHAR                   NO-UNDO.
DEF VAR lcBDestLimit AS CHAR                   NO-UNDO.
DEF VAR lcUsage      AS CHAR                   NO-UNDO.
DEF VAR lcLimit      AS CHAR                   NO-UNDO.
DEF VAR lcBillItem   AS CHAR                   NO-UNDO.
DEF VAR lcPeriodType AS CHAR                   NO-UNDO.
DEF VAR ldaFromDate  AS DATE                   NO-UNDO.
DEF VAR ldaEndDate   AS DATE                   NO-UNDO.
DEF VAR ldeStartingFees AS DECIMAL NO-UNDO. 
DEF VAR ldeUnitCharge   AS DECIMAL NO-UNDO. 
DEF VAR lcSLSeqs     AS CHAR NO-UNDO. 

form
    ldaFromDate              FORMAT 99-99-9999 Column-label "From"
    ldaEndDate               FORMAT 99-99-9999 Column-label "To"
    lcEventName              FORMAT "X(15)" Column-label "ServiceLimit" 
    lcInclUnit               FORMAT "X(12)" column-label "Unit"
    lcLimit                  FORMAT "x(8)" column-label "Limit"
    lcUsage                  FORMAT "x(8)" column-label "Usage"           
    lcBDestLimit             FORMAT "x(4)" column-label "BDestAmt"
WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
    " COUNTERS for period " + STRING(iiPeriod) + " " + icEvent + " "
    FRAME sel.

form
   "Service Limit Group:" ServiceLimitGroup.GroupCode 
                          ServiceLimitGroup.GroupName SKIP
   "Period type........:" lcPeriodType  SKIP
   "Counter period.....:" ServiceLCounter.Period SKIP
"----------------------------------------------------------------------------"
   SKIP 
   "Included unit.....:" servicelimit.inclunit lcInclUnit SKIP
   "Counter limit.....:" lcLimit SKIP
   "Usage...... ......:" lcUsage SKIP 
"----------------------------------------------------------------------------"
   "Billing Item.....:" lcBillItem FORMAT "x(50)" SKIP
"----------------------------------------------------------------------------"
   "Price list........:"   SKIP
   "Starting fees.. ..:" ldeStartingFees  SKIP
   "Unit charge.......:" ldeUnitCharge    SKIP
WITH OVERLAY ROW 2 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   " COUNTERS for period " + STRING(iiPeriod) + " " + icEvent + " "
   WITH no-labels side-labels
   FRAME lis.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By MSSeq   , By SLSeq ,By 3, By 4".

FOR EACH ServiceLimit WHERE
   ServiceLimit.GroupCode = icEvent NO-LOCK:
   lcSLSeqs = lcSLSeqs + STRING(ServiceLimit.SLSeq) + ",".
END.

RUN local-find-first.

IF AVAILABLE ServiceLCounter THEN ASSIGN
   memory       = recid(ServiceLCounter)
   must-print   = true
   must-add     = false.
ELSE DO:
   
   MESSAGE "No ServiceLCounter records available !" VIEW-AS ALERT-BOX.
   RETURN.
END.
LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a ServiceLCounter  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR ServiceLCounter.MSSeq
           validate
              (ServiceLCounter.MSSeq NOT ENTERED or
              NOT CAN-FIND(ServiceLCounter using  ServiceLCounter.MSSeq),
              "Billing Type " + string(INPUT ServiceLCounter.MSSeq) +
              " already exists !").
           IF INPUT FRAME lis ServiceLCounter.MSSeq = "" THEN 
           LEAVE add-row.
           create ServiceLCounter.
           ASSIGN
           ServiceLCounter.MSSeq = INPUT FRAME lis ServiceLCounter.MSSeq.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           memory = recid(ServiceLCounter)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ServiceLCounter THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND ServiceLCounter WHERE recid(ServiceLCounter) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ServiceLCounter THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(ServiceLCounter).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 0  ufk[6]= 0 
        ufk[7]= 925 
        ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row ldaFromDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ldaFromDate WITH FRAME sel.
      END.
      
      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND ServiceLCounter WHERE recid(ServiceLCounter) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE ServiceLCounter THEN
              ASSIGN FIRSTrow = i memory = recid(ServiceLCounter).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE ServiceLCounter THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(ServiceLCounter)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE ServiceLCounter THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(ServiceLCounter).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ServiceLCounter WHERE recid(ServiceLCounter) = memory 
        NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ServiceLCounter THEN DO:
           memory = recid(ServiceLCounter).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE ServiceLCounter THEN memory = recid(ServiceLCounter).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND ServiceLCounter WHERE recid(ServiceLCounter) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN DO TRANS:
       MESSAGE "This view is not yet implemented" VIEW-AS ALERT-BOX. 
       LEAVE. /* This view is not yet implemented */
/*
       /* change */
       RUN local-find-this(false).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. 
       
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       
       RUN local-disp-row.
       xrecid = recid(ServiceLCounter).
       LEAVE. */
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(ServiceLCounter) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(ServiceLCounter) must-print = true.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:
         RUN local-find-this(false).
         run slcounteritem.p(ServiceLCounter.msseq, 
                             ServiceLCounter.Period,
                             ServiceLCounter.SLSeq).
         ufkey = True.
         RUN local-disp-row.
         xrecid = recid(ServiceLCounter).
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    def INPUT parameter exlock as lo NO-undo.

    IF exlock THEN
      find ServiceLCounter WHERE recid(ServiceLCounter) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find ServiceLCounter WHERE recid(ServiceLCounter) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 AND iMSSeq ne 0 THEN 
          FIND FIRST ServiceLCounter WHERE
                     ServiceLCounter.MSSeq = iMSSeq  AND 
                     ServiceLCounter.Period > 0      AND
            (IF icEvent eq "" THEN TRUE
             ELSE LOOKUP(STRING(ServiceLCounter.SLSeq),lcSLSeqs) > 0) AND 
            (IF iiPeriod = 0 THEN TRUE 
             ELSE ServiceLCounter.Period = iiPeriod) AND
            (IF ServiceLCounter.MSID = 0 THEN TRUE
             ELSE ServiceLCounter.MSID = iiMSID) AND
            (IF islseq = 0 THEN TRUE 
             ELSE serviceLCounter.slseq = islseq) NO-LOCK NO-ERROR.

       ELSE IF order = 1 AND iiCustNum ne 0 THEN 
          FIND FIRST ServiceLCounter WHERE
                     ServiceLCounter.CustNum = iiCustNum AND 
                     ServiceLCounter.Period > 0          AND
            (IF icEvent eq "" THEN TRUE
             ELSE LOOKUP(STRING(ServiceLCounter.SLSeq),lcSLSeqs) > 0) AND 
            (IF iiPeriod = 0 THEN TRUE 
             ELSE ServiceLCounter.Period = iiPeriod) AND
            (IF ServiceLCounter.MSID = 0 THEN TRUE
             ELSE ServiceLCounter.MSID = iiMSID) AND
            (IF islseq = 0 THEN TRUE 
             ELSE serviceLCounter.slseq = islseq) NO-LOCK NO-ERROR.

       ELSE IF  order = 1  THEN FIND FIRST ServiceLCounter WHERE 
                                           ServiceLCounter.Period > 0      
        NO-LOCK NO-ERROR.              
END PROCEDURE.

PROCEDURE local-find-LAST:
      IF order = 1 AND iMSSeq ne 0 THEN
                 FIND LAST ServiceLCounter WHERE
                           ServiceLCounter.Msseq = imsseq AND
                           ServiceLCounter.Period > 0      AND
            (IF icEvent eq "" THEN TRUE
             ELSE LOOKUP(STRING(ServiceLCounter.SLSeq),lcSLSeqs) > 0) AND 
            (IF iiPeriod = 0 THEN TRUE 
             ELSE ServiceLCounter.Period = iiPeriod) AND
            (IF ServiceLCounter.MSID = 0 THEN TRUE
             ELSE ServiceLCounter.MSID = iiMSID) AND
            (IF islseq = 0 THEN TRUE 
             ELSE serviceLCounter.slseq = islseq) NO-LOCK NO-ERROR.
       
       ELSE IF order = 1 AND iiCustNum ne 0 THEN
                 FIND LAST ServiceLCounter WHERE
                           ServiceLCounter.CustNum = iiCustNum AND
                           ServiceLCounter.Period > 0          AND
            (IF icEvent eq "" THEN TRUE
             ELSE LOOKUP(STRING(ServiceLCounter.SLSeq),lcSLSeqs) > 0) AND 
            (IF iiPeriod = 0 THEN TRUE 
             ELSE ServiceLCounter.Period = iiPeriod) AND
            (IF ServiceLCounter.MSID = 0 THEN TRUE
             ELSE ServiceLCounter.MSID = iiMSID) AND
            (IF islseq = 0 THEN TRUE
             ELSE serviceLCounter.slseq = islseq) NO-LOCK NO-ERROR.

       ELSE IF  order = 1  THEN FIND LAST ServiceLCounter  WHERE 
                                          ServiceLCounter.Period > 0      
       NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 AND iMSSeq ne 0 THEN
       FIND NEXT ServiceLCounter WHERE
                 ServiceLCounter.Msseq = imsseq AND
                 ServiceLCounter.Period > 0      AND
            (IF icEvent eq "" THEN TRUE
             ELSE LOOKUP(STRING(ServiceLCounter.SLSeq),lcSLSeqs) > 0) AND 
            (IF iiPeriod = 0 THEN TRUE 
             ELSE ServiceLCounter.Period = iiPeriod) AND
            (IF ServiceLCounter.MSID = 0 THEN TRUE
             ELSE ServiceLCounter.MSID = iiMSID) AND
            (IF islseq = 0 THEN TRUE
             ELSE serviceLCounter.slseq = islseq) NO-LOCK NO-ERROR.
       
       ELSE IF order = 1 AND iiCustNum ne 0 THEN
       FIND NEXT ServiceLCounter WHERE
                 ServiceLCounter.CustNum = iiCustNum AND
                 ServiceLCounter.Period > 0          AND
            (IF icEvent eq "" THEN TRUE
             ELSE LOOKUP(STRING(ServiceLCounter.SLSeq),lcSLSeqs) > 0) AND 
            (IF iiPeriod = 0 THEN TRUE 
             ELSE ServiceLCounter.Period = iiPeriod) AND
            (IF ServiceLCounter.MSID = 0 THEN TRUE
             ELSE ServiceLCounter.MSID = iiMSID) AND
            (IF islseq = 0 THEN TRUE
             ELSE serviceLCounter.slseq = islseq) NO-LOCK NO-ERROR.

       ELSE IF  order = 1  THEN FIND NEXT  ServiceLCounter  WHERE 
        ServiceLCounter.Period > 0      
       NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 AND iMSSeq ne 0 THEN
       FIND PREV ServiceLCounter WHERE
                 ServiceLCounter.Msseq = imsseq AND
                  ServiceLCounter.Period > 0      AND
            (IF iiPeriod = 0 THEN TRUE 
             ELSE ServiceLCounter.Period = iiPeriod) AND
            (IF ServiceLCounter.MSID = 0 THEN TRUE
             ELSE ServiceLCounter.MSID = iiMSID) AND
            (IF icEvent eq "" THEN TRUE
             ELSE LOOKUP(STRING(ServiceLCounter.SLSeq),lcSLSeqs) > 0) AND 
        (IF islseq = 0 THEN TRUE
         ELSE serviceLCounter.slseq = islseq) NO-LOCK NO-ERROR.

       ELSE IF order = 1 AND iiCustNum ne 0 THEN
       FIND PREV ServiceLCounter WHERE
                 ServiceLCounter.CustNum = iiCustNum AND
                  ServiceLCounter.Period > 0         AND
            (IF iiPeriod = 0 THEN TRUE 
             ELSE ServiceLCounter.Period = iiPeriod) AND
            (IF ServiceLCounter.MSID = 0 THEN TRUE
             ELSE ServiceLCounter.MSID = iiMSID) AND
            (IF icEvent eq "" THEN TRUE
             ELSE LOOKUP(STRING(ServiceLCounter.SLSeq),lcSLSeqs) > 0) AND 
        (IF islseq = 0 THEN TRUE
         ELSE serviceLCounter.slseq = islseq) NO-LOCK NO-ERROR.

       ELSE IF  order = 1  THEN FIND NEXT  ServiceLCounter  WHERE 
        ServiceLCounter.Period > 0      
       NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE local-disp-row:
    RUN local-find-others.
    CLEAR FRAME sel NO-PAUSE.
    DISPLAY 
       ldaFromDate
       ldaEndDate
       lcEventName
       lcInclUnit
       lcLimit
       lcUsage 
       lcBDestLimit
    WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   
   DEF VAR lcFromTS AS DECIMAL NO-UNDO. 
   DEF VAR lcToTS   AS DECIMAL NO-UNDO. 
   DEF VAR ldPeriodFrom AS DEC  NO-UNDO.
   DEF VAR ldPeriodTo   AS DEC  NO-UNDO.
   DEF VAR ldeLimit  AS DECIMAL NO-UNDO.

   FIND FIRST Servicelimit WHERE 
              ServiceLimit.slseq = ServiceLCounter.SLSeq no-lock no-error.
   IF AVAIL Servicelimit THEN DO:
    
      FIND FIRST ServiceLimitGroup WHERE
         ServiceLimitGroup.Brand = gcBrand AND
         ServiceLimitGroup.GroupCode = ServiceLimit.GroupCode NO-LOCK NO-ERROR.
     
      ASSIGN 
        ldaFromDate = DATE(INT(SUBSTRING(STRING(iiPeriod),5,2)),
                           (IF iiPeriod > 999999 THEN 
                           INT(SUBSTRING(STRING(iiPeriod),7,2)) ELSE 1),
                           INT(SUBSTRING(STRING(iiPeriod),1,4)))
        ldaEndDate = (IF iiPeriod > 999999 THEN ldaFromDate 
                      ELSE fLastDayOfMonth(ldaFromDate))
        ldPeriodFrom = fMake2Dt(ldaFromDate,0)
        ldPeriodTo   = fMake2Dt(ldaEndDate,86399).

      IF ServiceLimitGroup.GroupCode BEGINS {&DSS} THEN
         FIND FIRST MServiceLimit WHERE
                    MServiceLimit.CustNum = ServiceLCounter.CustNum AND
                    MServiceLimit.SlSeq   = ServiceLCounter.SlSeq   AND
                    MServiceLimit.FromTS <= ldPeriodTo              AND
                    MServiceLimit.EndTS  >= ldPeriodFrom AND
                   (IF ServiceLCounter.MSID = 0 THEN TRUE
                    ELSE MServiceLimit.MSID = ServiceLCounter.MSID)
         NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST MServiceLimit WHERE
                    MServiceLimit.MsSeq   = ServiceLCounter.MsSeq AND
                    MServiceLimit.SlSeq   = ServiceLCounter.SlSeq AND
                    MServiceLimit.FromTS <= ldPeriodTo            AND
                    MServiceLimit.EndTS  >= ldPeriodFrom AND
                   (IF ServiceLCounter.MSID = 0 THEN TRUE
                    ELSE MServiceLimit.MSID = ServiceLCounter.MSID)
         NO-LOCK NO-ERROR.
      
      ASSIGN lcEventName = Servicelimit.SlCode
             ldeLimit    = mServiceLimit.InclAmt.

      /* Find latest MServiceLPool to get the limit value for upsells */
      IF ServiceLimitGroup.GroupCode MATCHES "*_UPSELL" THEN
         FIND FIRST MServiceLPool WHERE
                    MServiceLPool.MsSeq   = MServiceLimit.MsSeq AND
                    MServiceLPool.SlSeq   = MServiceLimit.SlSeq AND
                    MServiceLPool.FromTS <= ldPeriodTo          AND
                    MServiceLPool.EndTS  >= ldPeriodFrom NO-LOCK NO-ERROR.
      ELSE IF LOOKUP(ServiceLimitGroup.GroupCode,{&DSS_BUNDLES}) > 0 THEN
         FIND FIRST MServiceLPool WHERE
                    MServiceLPool.MsSeq   = MServiceLimit.MsSeq AND
                    MServiceLPool.SlSeq   = MServiceLimit.SlSeq AND
                    MServiceLPool.FromTS <= ldPeriodTo          AND
                    MServiceLPool.EndTS  >= ldPeriodTo NO-LOCK NO-ERROR.

      IF AVAILABLE MServiceLPool THEN ldeLimit = MServiceLPool.LimitAmt.

      /* Textual presentation for unit */
      FIND FIRST TMSCodes WHERE                                       
          TMSCodes.Tablename    = "ServiceLimit" AND               
          TMSCodes.FieldName    = "InclUnit" AND          
          TMSCodes.CodeGroup    = "ServiceLimit" AND        
          TMSCodes.CodeValue    = STRING(ServiceLimit.InclUnit)
      NO-LOCK NO-ERROR.                                               
      IF AVAIL TMSCodes THEN lcInclUnit = TMSCodes.CodeName.           
    
      CASE ServiceLimit.InclUnit:
         WHEN 1 THEN DO:
            lcUsage = fSec2C(ServiceLCounter.Amt,8).
            lcLimit = fSec2C(ldeLimit * 60,8).
         END.
         WHEN 2 THEN DO:
            lcUsage = fSec2C(ServiceLCounter.Amt,8).
            lcLimit = fSec2C(ldeLimit,8).
         END.
         WHEN 4 THEN DO:
            lcUsage = STRING(ROUND(ServiceLCounter.Amt / 1024 / 1024,2)).
            lcLimit = STRING(ldeLimit).
         END.
         OTHERWISE DO:
            lcUsage = STRING(ServiceLCounter.Amt).
            lcLimit = STRING(ldeLimit).
         END.
      END.
   
   END.   
   ELSE ASSIGN lcEventname = "" usedamt = "".           

   lcBDestLimit = 
      (IF ServiceLCounter.Limit > 0 THEN STRING(ServiceLCounter.Limit)
       ELSE "N/A").

END PROCEDURE.


PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      
      lcPeriodType = "2 Month". 
      ldeStartingFees = 0.
      ldeUnitCharge = 0.

      RUN local-find-others.
      PAUSE 0.
      DISP  
         ServiceLimitGroup.GroupCode
         ServiceLimitGroup.GroupName
         lcPeriodType
         serviceLCounter.period
         servicelimit.inclunit lcInclUnit
         lcLimit
         lcUsage
         lcBillItem
         ldeStartingFees
         ldeUnitCharge
      WITH FRAME lis. 

      MESSAGE "PRESS ENTER TO CONTINUE!". PAUSE NO-MESSAGE.
      
      LEAVE.
   END.
END PROCEDURE.
