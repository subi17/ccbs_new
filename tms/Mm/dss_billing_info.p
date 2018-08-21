/* ----------------------------------------------------------------------
  MODULE .......: dss_billing_info.p
  TASK .........: Display associated subscriptions and fee
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 16.09.11
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/extralinefunc.i}

DEF INPUT PARAMETER iiCustNum   AS INT NO-UNDO.

DEF TEMP-TABLE ttDSSInfo NO-UNDO
   FIELD MsSeq            AS INT
   FIELD CustNum          AS INT
   FIELD CLI              AS CHAR
   FIELD CLIType          AS CHAR
   FIELD CLIStatus        AS CHAR
   FIELD ActTS            AS DEC
   FIELD BundleId         AS CHAR
   FIELD BundleLimit      AS DEC
   FIELD BundleLimitinMB  AS DEC
   FIELD DataAllocated    AS DEC
   FIELD BundleFee        AS DEC
   FIELD BundleFromTS     AS DEC
   FIELD BundleEndTS      AS DEC
   FIELD SubsUsage        AS DEC
   FIELD BundleStatus     AS CHAR
   FIELD BundleFeeCalc    AS LOG
   FIELD UsedDSSData      AS DEC
   FIELD InclUnit         AS INT
   FIELD Priority         AS INT
   INDEX MsSeqBun MsSeq BundleId.

FIND FIRST Customer WHERE
           Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   MESSAGE "Customer not found: " + STRING(iiCustNum)
      VIEW-AS ALERT-BOX.
   RETURN.
END. /* IF NOT AVAILABLE Customer THEN DO: */


/* ************ Main Start **************/

RUN pGetDSSBillingInfo.


IF NOT CAN-FIND(FIRST ttDSSInfo) THEN DO:
   MESSAGE 
   "CANNOT FIND ANY DSS Information!"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS ROWID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS ROWID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.

DEF VAR lcMobSubActTS    AS CHAR               NO-UNDO.
DEF VAR lcBundleFromTS   AS CHAR               NO-UNDO.
DEF VAR lcBundleENDTS    AS CHAR               NO-UNDO.

DEF VAR lcIPLContracts                AS CHAR  NO-UNDO.
DEF VAR lcBONOContracts               AS CHAR  NO-UNDO.
DEF VAR lcAllowedDSS2SubsType         AS CHAR  NO-UNDO.
DEF VAR lcAllowedDSS4SubsType         AS CHAR  NO-UNDO. 
DEF VAR lcExcludeBundles              AS CHAR  NO-UNDO.
DEF VAR lcFirstMonthUsageBasedBundles AS CHAR  NO-UNDO.

{Mm/dss_bundle_first_month_fee.i}

form
    ttDSSInfo.CLI           FORMAT "X(9)"  LABEL "MSISDN"
    ttDSSInfo.CLIType       FORMAT "X(8)"  LABEL "Sub.Type"
    ttDSSInfo.BundleId      FORMAT "X(8)"   LABEL "Bundle"
    
    ttDSSInfo.BundleLimitinMB  FORMAT ">>>>>>9" LABEL "Limit(MB)"
    ttDSSInfo.SubsUsage                       LABEL "Usage(MB)"
    ttDSSInfo.DataAllocated LABEL "Alloc.(MB)" 
    ttDSSInfo.BundleFee  FORMAT ">>9.99" LABEL "B.Fee"
    ttDSSInfo.Priority      FORMAT ">>9"  LABEL "Pr."
    ttDSSInfo.BundleFeeCalc FORMAT "Yes/No" LABEL "PMF" 
WITH ROW FrmRow width 78 centered OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " +
       " DSS Billing Information "  + string(TODAY,"99-99-99") + " "
    FRAME sel.

form
    ttDSSInfo.CLI           COLON 20 FORMAT "X(10)"   LABEL "MSISDN"
    ttDSSInfo.CLIType       COLON 20 FORMAT "X(10)"   LABEL "CLI Type"
    lcMobSubActTS           COLON 20 FORMAT "X(20)"   LABEL "Subs. Activation"
    ttDSSInfo.CLIStatus     COLON 20 FORMAT "x(10)"   LABEL "Subs. Status"
    ttDSSInfo.BundleId      COLON 20 FORMAT "X(20)"   LABEL "Bundle ID"
    lcBundleFromTS          COLON 20 FORMAT "X(20)"   LABEL "Bundle Activation"
    lcBundleENDTS           COLON 20 FORMAT "X(20)"   LABEL "Bundle End Date"
    ttDSSInfo.BundleStatus  COLON 20 FORMAT "X(20)"   LABEL "Bundle Status"
    ttDSSInfo.BundleLimitinMB COLON 20                LABEL "Bundle Limit"
    ttDSSInfo.SubsUsage     COLON 20                  LABEL "Bundle Usage"
    ttDSSInfo.Priority      COLON 20                  LABEL "DSS Data Priority"
    ttDSSInfo.DataAllocated COLON 20                  LABEL "DSS Data Allocated"
    ttDSSInfo.BundleFeeCalc COLON 20                  LABEL "First Month Fee"
    ttDSSInfo.BundleFee     COLON 20                  LABEL "Bundle Fee"
WITH  OVERLAY ROW 3 width 50 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
view FRAME sel.

RUN local-find-first.
       
IF AVAILABLE ttDSSInfo THEN ASSIGN
   Memory       = ROWID(ttDSSInfo)
   must-print   = TRUE.
ELSE DO:
    MESSAGE 
    "NOT ALLOWED DSS Information!"
    VIEW-AS ALERT-BOX.
    RETURN.
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
        FIND ttDSSInfo WHERE ROWID(ttDSSInfo) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose ROWID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttDSSInfo THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = ROWID(ttDSSInfo).
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
           Syst.Var:ufk    = 0
           Syst.Var:ufk[8] = 8
           Syst.Var:ehto   = 3 
           ufkey  = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttDSSInfo.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) ttDSSInfo.CLI WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW ttDSSInfo.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) ttDSSInfo.CLI WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ttDSSInfo WHERE ROWID(ttDSSInfo) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttDSSInfo THEN
              ASSIGN FIRSTrow = i Memory = ROWID(ttDSSInfo).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttDSSInfo THEN DO:
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
                rtab[1] = ROWID(ttDSSInfo)
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
           IF NOT AVAILABLE ttDSSInfo THEN DO:
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
              rtab[FRAME-DOWN] = ROWID(ttDSSInfo).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttDSSInfo WHERE ROWID(ttDSSInfo) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttDSSInfo THEN DO:
           Memory = ROWID(ttDSSInfo).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttDSSInfo THEN Memory = ROWID(ttDSSInfo).
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
           FIND ttDSSInfo WHERE ROWID(ttDSSInfo) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE Syst.Var:ehto = 5. RUN Syst/ufkey.p.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
       
       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       RUN local-disp-row.
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = ROWID(ttDSSInfo) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = ROWID(ttDSSInfo) must-print = TRUE.
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
      FIND ttDSSInfo WHERE ROWID(ttDSSInfo) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttDSSInfo WHERE ROWID(ttDSSInfo) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST ttDSSInfo NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
       FIND LAST ttDSSInfo NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT ttDSSInfo NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN 
      FIND PREV ttDSSInfo NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:

    RUN local-find-others.
       
    CLEAR FRAME sel NO-PAUSE.

    DISP ttDSSInfo.CLI
         ttDSSInfo.CLIType
         ttDSSInfo.BundleId
         ttDSSInfo.BundleLimitinMB 
         ttDSSInfo.SubsUsage
      (ttDSSInfo.DataAllocated / 1024 / 1024) @ ttDSSInfo.DataAllocated 
         ttDSSInfo.Priority
         ttDSSInfo.BundleFeeCalc 
         ttDSSInfo.BundleFee
    WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

  REPEAT ON ENDKEY UNDO, LEAVE:

     RUN local-find-others.

     ASSIGN lcMobSubActTS  = Func.Common:mTS2HMS(ttDSSInfo.ActTS)
            lcBundleFromTS = Func.Common:mTS2HMS(ttDSSInfo.BundleFromTS).

     IF ttDSSInfo.BundleEndTS = 99999999.99999 THEN
        lcBundleENDTS  = Func.Common:mTS2HMS(20491231.86399).
     ELSE
        lcBundleENDTS  = Func.Common:mTS2HMS(ttDSSInfo.BundleEndTS).

     DISP ttDSSInfo.CLI
          ttDSSInfo.CLIType
          lcMobSubActTS
          ttDSSInfo.CLIStatus
          ttDSSInfo.BundleId
          lcBundleFromTS
          lcBundleENDTS
          ttDSSInfo.BundleStatus
          ttDSSInfo.BundleLimitinMB
          ttDSSInfo.SubsUsage
          ttDSSInfo.BundleFee
          ttDSSInfo.BundleFeeCalc  
      (ttDSSInfo.DataAllocated / 1024 / 1024) @ ttDSSInfo.DataAllocated 
         ttDSSInfo.Priority
     WITH FRAME lis.

     ASSIGN 
         Syst.Var:ehto = 0
         Syst.Var:ufk  = 0
         Syst.Var:ufk[8] = 8.
     RUN Syst/ufkey.p.

     IF Syst.Var:toimi = 8 THEN LEAVE.

  END.

END PROCEDURE.


PROCEDURE pGetDSSBillingInfo:

   DEF VAR ldeDSSUsage              AS DEC  NO-UNDO.
   DEF VAR ldeBundleAmt             AS DEC  NO-UNDO.
   DEF VAR ldPeriodFrom             AS DEC  NO-UNDO.
   DEF VAR ldPeriodTo               AS DEC  NO-UNDO.
   DEF VAR ldeFeeAmt                AS DEC  NO-UNDO.
   DEF VAR ldeBundleInclAmt         AS DEC  NO-UNDO.
   DEF VAR ldFromDate               AS DATE NO-UNDO.
   DEF VAR ldToDate                 AS DATE NO-UNDO.
   DEF VAR ldFFItemStartDate        AS DATE NO-UNDO.
   DEF VAR liFFItemStartTime        AS INT  NO-UNDO.
   DEF VAR liPeriod                 AS INT  NO-UNDO.
   DEF VAR llFullMonth              AS LOG  NO-UNDO.
   DEF VAR ldeDSSLimit              AS DEC  NO-UNDO. 
   DEF VAR ldeDataAllocated         AS DEC  NO-UNDO.
   DEF VAR lcBundleId               AS CHAR NO-UNDO.
   DEF VAR lcDSSBundleId            AS CHAR NO-UNDO. 

   DEF BUFFER bMServiceLimit        FOR MServiceLimit.
   DEF BUFFER bServiceLimit         FOR ServiceLimit.
   DEF BUFFER bDayCampaign          FOR DayCampaign.
   DEF BUFFER bMsRequest            FOR MsRequest.

   ASSIGN liPeriod     = YEAR(TODAY) * 100 + MONTH(TODAY)
          ldFromDate   = DATE(MONTH(today), 1, YEAR(today))
          ldToDate     = TODAY 
          ldPeriodFrom = Func.Common:mMake2DT(ldFromDate,0)
          ldPeriodTo   = Func.Common:mMake2DT(ldToDate,86399).

   /* Check wheather customer is linked with DSS service or not */
   lcBundleId = fGetActiveDSSId(Customer.CustNum,ldPeriodTo).
   IF lcBundleId = "" THEN DO:
      MESSAGE "DSS bundle is not active." VIEW-AS ALERT-BOX.
      RETURN.
   END. /* IF lcBundleId = "" THEN DO: */
          
   ldeDSSUsage = fGetDSSUsage(INPUT Customer.CustNum,
                              INPUT TODAY,
                              OUTPUT ldeDSSLimit).

   ASSIGN lcIPLContracts                = fCParamC("IPL_CONTRACTS")
          lcBONOContracts               = fCParamC("BONO_CONTRACTS")
          lcExcludeBundles              = fCParamC("EXCLUDE_BUNDLES")
          lcAllowedDSS2SubsType         = fCParamC("DSS2_SUBS_TYPE")
          lcAllowedDSS4SubsType         = fCParamC("DSS4_SUBS_TYPE")
          lcFirstMonthUsageBasedBundles = fCParamC("FIRST_MONTH_USAGE_BASED_BUNDLES").

   fGetMsOwnerTempTable(Customer.CustNum,ldFromDate,ldToDate,FALSE,FALSE).

   FOR EACH ttMsOwner BREAK BY ttMsOwner.MsSeq:

      IF lcBundleId EQ {&DSS4} AND 
         LOOKUP(ttMsOwner.CLIType,lcAllowedDSS4SubsType) = 0 THEN NEXT.
      ELSE IF lcBundleId EQ {&DSS2} AND
         LOOKUP(ttMsOwner.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.

      IF fCLITypeIsMainLine(ttMsOwner.CLIType) AND
         NOT fCheckActiveExtraLinePair(ttMsOwner.MsSeq,
                                       ttMsOwner.CLIType,
                                       OUTPUT lcDSSBundleId) THEN
         NEXT.      

      FOR EACH bMServiceLimit WHERE
               bMServiceLimit.MsSeq    = ttMsOwner.MsSeq          AND
               bMServiceLimit.DialType = {&DIAL_TYPE_GPRS}        AND
               bMServiceLimit.FromTS  <= ttMsOwner.PeriodTo       AND
              (bMServiceLimit.EndTS   >= ttMsOwner.PeriodFrom AND 
               bMServiceLimit.EndTS   >= ttMsOwner.PeriodTo   AND
               bMServiceLimit.EndTS   >= Func.Common:mMakeTS())   NO-LOCK,
         FIRST bServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
               bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
         FIRST bDayCampaign NO-LOCK WHERE
               bDayCampaign.Brand = Syst.Var:gcBrand AND
               bDayCampaign.DCEvent = bServiceLimit.GroupCode:

         IF CAN-FIND(FIRST ttDSSInfo WHERE
                           ttDSSInfo.MsSeq    = bMServiceLimit.MsSeq AND
                           ttDSSInfo.BundleId = bServiceLimit.GroupCode) OR
            LOOKUP(bServiceLimit.GroupCode,lcExcludeBundles) > 0
         THEN NEXT.

         ldeBundleInclAmt = bMServiceLimit.InclAmt.

         ASSIGN ldeBundleAmt = (ldeBundleInclAmt * 1024 * 1024)
                llFullMonth  = FALSE
                ldeDataAllocated = 0.

         Create ttDSSInfo.
         ASSIGN ttDSSInfo.MsSeq           = bMServiceLimit.MsSeq
                ttDSSInfo.CustNum         = ttMsOwner.CustNum
                ttDSSInfo.CLIType         = ttMsOwner.CLIType
                ttDSSInfo.CLI             = ttMsOwner.CLI
                ttDSSInfo.BundleId        = bServiceLimit.GroupCode
                ttDSSInfo.BundleFromTS    = bMServiceLimit.FromTS
                ttDSSInfo.BundleEndTS     = bMServiceLimit.EndTS
                ttDSSInfo.BundleLimit     = ldeBundleAmt
                ttDSSInfo.BundleLimitInMB = ldeBundleInclAmt
                ttDSSInfo.InclUnit        = bServiceLimit.InclUnit
                ttDSSInfo.BundleStatus    = "Active".

         /* DSS/Upsell usage */
         IF bServiceLimit.GroupCode BEGINS {&DSS} THEN DO:
            FIND FIRST ServiceLCounter WHERE
                       ServiceLCounter.CustNum = bMServiceLimit.CustNum AND
                       ServiceLCounter.SLSeq   = bServiceLimit.SlSeq  AND
                       ServiceLCounter.Period  = liPeriod NO-LOCK NO-ERROR.
            IF AVAIL ServiceLCounter THEN
               ttDSSInfo.SubsUsage = (ServiceLCounter.Amt / 1024 / 1024).
         END. /* IF bServiceLimit.GroupCode BEGINS {&DSS} THEN DO: */
         /* Get Subs. level usage before DSS activation */
         ELSE DO:
            FIND FIRST ServiceLCounter WHERE
                       ServiceLCounter.MsSeq  = bMServiceLimit.MsSeq AND
                       ServiceLCounter.SLSeq  = bServiceLimit.SlSeq  AND
                       ServiceLCounter.Period = liPeriod NO-LOCK NO-ERROR.
            IF AVAIL ServiceLCounter THEN
               ttDSSInfo.SubsUsage = (ServiceLCounter.Amt / 1024 / 1024).

            IF bMServiceLimit.InclAmt < ttDSSInfo.SubsUsage THEN
               ttDSSInfo.SubsUsage = bMServiceLimit.InclAmt.
         END. /* ELSE DO: */

         FIND FIRST MobSub WHERE
                    MobSub.MsSeq = ttMsOwner.MsSeq NO-LOCK NO-ERROR.
         IF AVAILABLE MobSub THEN
            ASSIGN ttDSSInfo.CLIStatus = "Active"
                   ttDSSInfo.ActTS     = MobSub.ActivationTS.
         ELSE DO:
            FIND FIRST TermMobSub WHERE
                       TermMobSub.MsSeq = ttMsOwner.MsSeq NO-LOCK NO-ERROR.
            IF AVAILABLE TermMobSub THEN
               ASSIGN ttDSSInfo.CLIStatus = "Terminated"
                      ttDSSInfo.ActTS     = TermMobSub.ActivationTS.
         END. /* ELSE DO: */

         /* Don't execute Upsells further */
         IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO:
            FIND FIRST MServiceLPool WHERE
                       MserviceLPool.MsSeq   = bMServiceLimit.MsSeq   AND
                       MserviceLPool.SLSeq   = bMServiceLimit.SLSeq   AND
                       MserviceLPool.FromTS <= ttMsOwner.PeriodTo     AND
                       MserviceLPool.EndTS  >= ttMsOwner.PeriodFrom NO-LOCK NO-ERROR.
            IF AVAILABLE MserviceLPool THEN
               ttDSSInfo.BundleLimitInMB = MserviceLPool.LimitAmt.
            NEXT.
         END. /* IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO: */

         /* If ongoing bundle termination request except BTC Request   */
         /* OR NOT FIRST month then reduce data amount from total DSS  */
         /* limit  because bundle will be charged as full month amt    */
         IF CAN-FIND(FIRST MSRequest WHERE
                     MSRequest.MsSeq = bMServiceLimit.MsSeq AND
                     MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                     MsRequest.ActStamp >= ldPeriodfrom AND
                     MsRequest.ActStamp <= ldPeriodTo AND
                     MsRequest.ReqCParam3 = bDayCampaign.DCEvent AND
                     MsRequest.ReqSource <> {&REQUEST_SOURCE_BTC} AND
                     LOOKUP(STRING(MsRequest.ReqStat),
                            {&REQ_INACTIVE_STATUSES}) = 0 USE-INDEX MsSeq) OR
            (LOOKUP(bDayCampaign.DCEvent,{&DSS_BUNDLES}) > 0 AND
             fOngoingDSSTerm(bMServiceLimit.CustNum,ldPeriodTo)) THEN
            llFullMonth = TRUE.

         IF bMServiceLimit.FromTS < ldPeriodFrom THEN
            llFullMonth = TRUE.
      
         /* if the data bundle contract is originating from STC/BTC
            then skip first month fee calculation */
         IF llFullMonth = FALSE AND
            bMServiceLimit.FromTs >= ldPeriodFrom THEN
            FOR EACH MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = ttMsOwner.MsSeq AND
                     MsRequest.ReqType = 8 AND
                     MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                     MsRequest.ActStamp >= ldPeriodFrom AND
                     MsRequest.ActStamp <= ldPeriodTo AND
                     MsRequest.OrigRequest > 0 AND
                     LOOKUP(STRING(MsRequest.ReqStatus),"2,9") > 0
                     USE-INDEX MsSeq,
               FIRST bMsRequest NO-LOCK WHERE
                     bMsRequest.MsRequest = MsRequest.OrigRequest:
               IF (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR 
                   bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) AND
                   bMsRequest.ActStamp = ldPeriodFrom THEN DO:
                  llFullMonth = TRUE.
                  LEAVE.
               END.
            END.

         /* First and last month is always usage based */
         IF LOOKUP(bDayCampaign.DCEvent,lcFirstMonthUsageBasedBundles) > 0 AND
            (bMServiceLimit.FromTs >= ldPeriodFrom OR
             bMServiceLimit.EndTs < ldPeriodTo) THEN .
         /* If Termination request is part of BTC */
         ELSE IF llFullMonth = FALSE AND
            bMServiceLimit.FromTs >= ldPeriodFrom AND
            CAN-FIND(FIRST MSRequest WHERE
                     MSRequest.MsSeq = bMServiceLimit.MsSeq AND
                     MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                     MsRequest.ActStamp >= ldPeriodFrom AND
                     MsRequest.ActStamp <= ldPeriodTo AND
                     MsRequest.ReqCParam3 = bDayCampaign.DCEvent AND
                     (LOOKUP(MsRequest.ReqCParam3,lcBONOContracts) > 0  OR
                      LOOKUP(MsRequest.ReqCParam3,lcIPLContracts)  > 0) AND
                     MsRequest.ReqSource = {&REQUEST_SOURCE_BTC} USE-INDEX MsSeq) THEN
            llFullMonth = FALSE.
         ELSE IF LOOKUP(bDayCampaign.DCEvent,{&DSS_BUNDLES}) > 0 THEN DO:
            IF bMServiceLimit.EndTS = ldPeriodTo AND
               CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                         MsRequest.Brand = Syst.Var:gcBrand           AND
                         MsRequest.ReqType = {&REQTYPE_DSS}  AND
                         MsRequest.Custnum = ttMsOwner.Custnum AND
                         MsRequest.ReqCParam1 = "DELETE"     AND
                         MsRequest.ActStamp   = ldPeriodTo   AND
                         MsRequest.ReqStatus  = {&REQUEST_STATUS_DONE}) THEN
               llFullMonth = TRUE.
         END.

         /* if the data bundle contract is terminated from STC/BTC
            in middle of month then skip first month fee calculation */
         IF llFullMonth = TRUE AND
            bMServiceLimit.EndTs < ldPeriodTo THEN
            FOR EACH MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = ttMsOwner.MsSeq AND
                     MsRequest.ReqType = 9 AND
                     MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                     MsRequest.ActStamp >= ldPeriodFrom AND
                     MsRequest.ActStamp < ldPeriodTo AND
                     MsRequest.OrigRequest > 0 AND
                     LOOKUP(STRING(MsRequest.ReqStatus),"2,9") > 0
                     USE-INDEX MsSeq,
               FIRST bMsRequest NO-LOCK WHERE
                     bMsRequest.MsRequest = MsRequest.OrigRequest:
               IF (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
                   bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) AND
                   bMsRequest.ActStamp < ldPeriodTo THEN DO:
                  llFullMonth = FALSE.
                  LEAVE.
               END.
            END.

         IF bMServiceLimit.EndTS <= ldPeriodTo THEN
            ttDSSInfo.BundleStatus = "Terminated".
         ELSE
            ttDSSInfo.BundleStatus = "Active".

         IF llFullMonth AND NOT bDayCampaign.DCEvent BEGINS {&DSS} THEN DO:
            IF ldeDSSUsage >= ldeBundleAmt THEN
                ASSIGN ldeDSSUsage = (ldeDSSUsage - ldeBundleAmt)
                       ldeDataAllocated = ldeBundleAmt.
            ELSE
               ASSIGN ldeDataAllocated = ldeDSSUsage
                      ldeDSSUsage = 0.
         END. /* IF CAN-FIND(FIRST MSRequest WHERE */

         ASSIGN 
            ttDSSInfo.DataAllocated   = ldeDataAllocated 
            ttDSSInfo.BundleFeeCalc   = (NOT llFullMonth)
            ttDSSInfo.Priority        = bDayCampaign.DSSPriority.

         IF llFullMonth THEN DO:
            /* for each used because there might exist  
               same type of fixed fee in the past */
            FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE
                     FixedFee.Brand     = Syst.Var:gcBrand AND
                     FixedFee.HostTable = "MobSub" AND
                     FixedFee.KeyValue  = STRING(ttDSSInfo.MsSeq) AND
                     FixedFee.FeeModel  = bDayCampaign.FeeModel AND
                     FixedFee.CalcObj   = bDayCampaign.DCEvent AND
                     FixedFee.InUse     = TRUE AND
                     FixedFee.BegDate  <= ldToDate,
               FIRST FFItem NO-LOCK WHERE
                     FFItem.FFNum = FixedFee.FFNum AND
                     FFItem.BillPeriod = liPeriod:

               ttDSSInfo.BundleFee = FixedFee.Amt.
               LEAVE.
            END. /* FOR FIRST DayCampaign NO-LOCK WHERE*/
         END. /* IF llFullMonth THEN DO: */
      END. /* FOR EACH bMServiceLimit WHERE */
             
      /* Make entry for a subscription without any bundle */
      IF NOT CAN-FIND(FIRST ttDSSInfo WHERE
                            ttDSSInfo.MsSeq = ttMsOwner.MsSeq) THEN DO:
         
         IF (LOOKUP(ttMsOwner.CLIType,lcAllowedDSS4SubsType) > 0 OR 
             LOOKUP(ttMsOwner.CLIType,lcAllowedDSS2SubsType) > 0)   AND 
            (fCLITypeIsMainLine(ttMsOwner.CLIType)               OR 
             fCLITypeIsExtraLine(ttMsOwner.CLIType))                THEN 
            IF NOT fCheckActiveExtraLinePair(ttMsOwner.MsSeq,
                                             ttMsOwner.CLIType,
                                             OUTPUT lcDSSBundleId) THEN 
               NEXT.

         CREATE ttDSSInfo.
         ASSIGN ttDSSInfo.MsSeq           = ttMsOwner.MsSeq
                ttDSSInfo.CustNum         = ttMsOwner.CustNum
                ttDSSInfo.CLIType         = ttMsOwner.CLIType
                ttDSSInfo.CLI             = ttMsOwner.CLI
                ttDSSInfo.BundleFeeCalc   = False
                ttDSSInfo.BundleStatus    = "N/A".
      END. /* IF NOT CAN-FIND(FIRST ttDSSInfo WHERE */
   END. /* FOR EACH ttMsOwner NO-LOCK WHERE */

   /* Calculate first month fee */
   FOR EACH ttDSSInfo WHERE
            ttDSSInfo.BundleFeeCalc = TRUE
            BY ttDSSInfo.Priority:

      ASSIGN ldeFeeAmt = 0
             ldeDataAllocated = 0.

      FOR FIRST DayCampaign NO-LOCK WHERE
                DayCampaign.Brand   = Syst.Var:gcBrand AND
                DayCampaign.DCEvent = ttDSSInfo.BundleId,
          FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
                FixedFee.Brand     = Syst.Var:gcBrand AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue  = STRING(ttDSSInfo.MsSeq) AND
                FixedFee.FeeModel  = DayCampaign.FeeModel AND
                FixedFee.CalcObj   = DayCampaign.DCEvent AND
                FixedFee.InUse     = TRUE AND
                FixedFee.BegDate  <= ldToDate   AND
                FixedFee.EndPer   >= liPeriod,
          FIRST FMItem NO-LOCK WHERE
                FMItem.Brand     = Syst.Var:gcBrand AND
                FMItem.FeeModel  = FixedFee.FeeModel AND
                FMItem.FromDate <= FixedFee.BegDate  AND
                FMItem.ToDate   >= FixedFee.BegDate:

          /* already billed */
          FIND FIRST FFItem OF FixedFee WHERE
                     FFItem.BillPeriod = liPeriod NO-LOCK NO-ERROR.
          IF NOT AVAILABLE FFItem THEN NEXT.

          /* % of fee, based on usage */
          IF FMItem.FirstMonthBr = 2 THEN DO:
             IF ldeDSSUsage > 0 THEN
                ldeFeeAmt = fCalculateProportionalFee(
                                    (IF ldeDSSUsage < ttDSSInfo.BundleLimit
                                     THEN ldeDSSUsage
                                     ELSE ttDSSInfo.BundleLimit),
                                    ttDSSInfo.InclUnit,
                                    ttDSSInfo.BundleLimitInMB,
                                    FixedFee.Amt).
             ELSE ldeFeeAmt = 0.
          END. /* ELSE IF FMItem.FirstMonthBr = 2 THEN DO: */
          ELSE ldeFeeAmt = FFItem.Amt.
      END. /* FOR FIRST DayCampaign NO-LOCK WHERE */

      IF NOT ttDSSInfo.BundleId BEGINS {&DSS} AND ldeDSSUsage > 0 THEN DO:
         IF ldeDSSUsage > ttDSSInfo.BundleLimit THEN 
            ldeDataAllocated = ttDSSInfo.BundleLimit.
         ELSE ldeDataAllocated = ldeDSSUsage.

         ldeDSSUsage = (ldeDSSUsage - ttDSSInfo.BundleLimit).
      END. /* IF ttDSSInfo.BundleId <> {&DSS} THEN DO: */

      /* Update Bundle Fee amount */
      ASSIGN
          ttDSSInfo.BundleFee = ldeFeeAmt
          ttDSSInfo.DataAllocated = ldeDataAllocated.  
   END. /* FOR EACH ttDSSInfo */

END PROCEDURE.
