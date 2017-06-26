{Syst/tmsconst.i}
{Func/fdss.i}

DEF TEMP-TABLE ttServiceLCounter NO-UNDO
   LIKE ServiceLCounter USE-INDEX msseq USE-INDEX Custnum.

DEF TEMP-TABLE ttSLCounterItem NO-UNDO
   LIKE SLCounterItem USE-INDEX msseq
   FIELD Picked AS INT.
   
FUNCTION fConvertAmountUnit RETURNS DECIMAL
   (iiUnit   AS INT,
    idAmount AS DEC):

   CASE iiUnit:
   WHEN {&INCLUNIT_MINUTE} THEN idAmount = idAmount.
   WHEN {&INCLUNIT_SECOND} THEN idAmount = idAmount.
   WHEN {&INCLUNIT_GIGABYTE} THEN idAmount = idAmount.
   WHEN {&INCLUNIT_MEGABYTE} THEN idAmount = idAmount.
   WHEN {&INCLUNIT_QUANTITY} THEN idAmount = 1.
   OTHERWISE idAmount = idAmount.
   END CASE.

   RETURN idAmount.
   
END FUNCTION.

FUNCTION  fTriggerEvent RETURN LOGICAL
(INPUT CallTimestamp AS DEC,
 INPUT iiMSSeq       AS INT,
 INPUT ideLatest     AS DEC):

   FIND FIRST TriggerConf WHERE
              TriggerConf.TriggerConfID = "MobCDR"        AND
              TriggerConf.EventRule     > 0               AND
              TriggerConf.ValidTo       >= Today          AND
              TriggerConf.ValidFrom     <= Today NO-LOCK NO-ERROR.
                                                              
   IF AVAIL TriggerConf THEN DO:
   
      CREATE TriggerEvent.
      ASSIGN
      TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
      TriggerEvent.TriggerConfID  = "MobCDR"
      TriggerEvent.EventSource    = "MODIFY"
      TriggerEvent.Created        = DateTime(Today,mtime)
      TriggerEvent.TableID        = iiMSSEQ
      TriggerEvent.TableName      = "Mobcdr"
      TriggerEvent.Keyvalue       = STRING(CallTimeStamp) 
      TriggerEvent.ChangedFields  = "ServicelCounter"
      TriggerEvent.ChangedValues  = STRING(idelatest).

      RELEASE TriggerEvent.
   END.
                
END FUNCTION.

FUNCTION fIsServiceLimitAllowed RETURNS LOG
   (INPUT  iiMSID         AS INT,
    INPUT  msseq          AS INT,
    INPUT  iicustnum      AS INT,
    INPUT  InvSeq         AS INT,
    INPUT  iislseq        AS INT,
    INPUT  iiInclUnit     AS INT,
    INPUT  idInclAmt      AS DEC,
    INPUT  CallTimeStamp  AS DEC,
    INPUT  iiDCType       AS INT,
    INPUT  iiBDestLimit   AS INT,
    INPUT  iiBDestAmt     AS INT,
    INPUT-OUTPUT  ideAmt  AS DEC).
                           
   DEF VAR liperiod       AS INT  NO-UNDO.
   DEF VAR SLimitAlloWed  AS INT  NO-UNDO.
   DEF VAR liQty          AS INT  NO-UNDO.
   DEF VAR ldeLimitAmt    AS DEC  NO-UNDO. 
                                       
   ASSIGN 
      liQty    = 0 
      liPeriod = INT(substring(STRING(CallTimeStamp),1,6))
      SLimitAlloWed = {&SLANALYSE_NO_PACKET}.

   DO WHILE TRUE :
   
      IF iicustnum > 0 THEN
         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.Custnum = iicustnum           AND
                    serviceLCounter.SLseq   = iiSlseq             AND 
                    ServiceLCounter.Period  = liPeriod            AND
                    ServiceLCounter.MsId    = iiMSID              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      ELSE
         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.Msseq   = msseq               AND
                    serviceLCounter.SLseq   = iiSlseq             AND 
                    ServiceLCounter.Period  = liPeriod            AND
                    ServiceLCounter.MsId    = iiMSID              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      liQty = liQty + 1.
      
      IF liQty > 30 THEN LEAVE.
       
      IF LOCKED(servicelcounter) THEN DO:
         PAUSE 1 NO-MESSAGE.
         NEXT.                             
      END.                       

      /* NEW ServiceLimit Counter */
      IF NOT AVAIL ServiceLCounter THEN DO:

         CREATE ServiceLCounter.
         ASSIGN
            ServiceLCounter.MSID   = iiMSID 
            ServiceLCounter.InvSeq = InvSeq
            ServiceLCounter.msseq  = (IF iiCustnum > 0 THEN 0 ELSE msseq)
            ServiceLCounter.Custnum = iiCustnum
            ServiceLCounter.slseq  = iiSlseq
            ServiceLCounter.Period = liPeriod 
            ServiceLCounter.Latest = 0
            ServiceLCounter.amt    = 0
            SLimitAllowed          = {&SLANALYSE_FULL_PACKET} NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
         ELSE LEAVE.
      END.
      LEAVE.
   END.   

   /* update ServiceLimit counter */
   IF Avail ServiceLCounter THEN DO:

      /* Populate InvSeq if it was not populated before for DSS */
      IF iicustnum > 0 AND
         (ServiceLCounter.InvSeq = 0 OR ServiceLCounter.InvSeq = ?) THEN
         ServiceLCounter.InvSeq = InvSeq.
      
      IF iiDCType EQ 6 THEN DO:
         IF iicustnum > 0 THEN
            FIND FIRST MServiceLPool WHERE
                       MServiceLPool.Custnum = iiCustnum AND
                       MServiceLPool.SlSeq = iiSlseq AND
                       MserviceLPool.EndTS >= CallTimeStamp AND
                       MserviceLPool.fromTS <= CallTimeStamp NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST MServiceLPool WHERE
                       MServiceLPool.MsSeq = msseq AND
                       MServiceLPool.SlSeq = iiSlseq AND
                       MserviceLPool.EndTS >= CallTimeStamp AND
                       MserviceLPool.fromTS <= CallTimeStamp NO-LOCK NO-ERROR.

         IF AVAIL MServiceLPool THEN ldeLimitAmt = MServiceLPool.LimitAmt.
         ELSE RETURN FALSE.
      END.
      ELSE ldeLimitAmt = idInclAmt.

      ideAmt = fConvertAmountUnit(iiInclUnit,ideAmt).

      CASE iiInclUnit:
         WHEN {&INCLUNIT_MEGABYTE} THEN 
            ldeLimitAmt = ldeLimitAmt * 1024 * 1024.
         WHEN {&INCLUNIT_GIGABYTE} THEN
            ldeLimitAmt = ldeLimitAmt * 1024 * 1024 * 1024.
         WHEN {&INCLUNIT_MINUTE} THEN 
            ldeLimitAmt = ldeLimitAmt * 60.
      END.
       
      ServiceLCounter.limit = ServiceLCounter.limit + iiBDestAmt.
   
      IF ServiceLCounter.amt < ldeLimitAmt AND
         ServiceLCounter.limit <= iiBDestLimit THEN DO:

         ASSIGN
            ServiceLCounter.amt = ServiceLCounter.amt + ideAmt
            SLimitAllowed       = {&SLANALYSE_FULL_PACKET}.

         /* what is left after this */
         IF ServiceLCounter.Amt > ldeLimitAmt THEN ASSIGN
            ideAmt              = ServiceLCounter.Amt - ldeLimitAmt
            SLimitAllowed       = {&SLANALYSE_BROKEN_PACKET} 
            ServiceLCounter.Amt = ldeLimitAmt.
         ELSE ideAmt = 0.   
      END.      
      ELSE SLimitAllowed = {&SLANALYSE_NO_PACKET}.
           
      IF (SLimitAllowed = {&SLANALYSE_NO_PACKET}     AND ServicelCounter.Latest > CallTimeStamp) OR 
         (SLimitAllowed = {&SLANALYSE_BROKEN_PACKET} AND ServicelCounter.Latest > CallTimeStamp)
      THEN DO:
         fTriggerEvent(INPUT CallTimeStamp,
                       ServicelCounter.MSSeq, 
                       ServicelCounter.Latest).
      END.
      ELSE IF 
         (SLimitAllowed =  {&SLANALYSE_FULL_PACKET} OR
          SLimitAllowed = {&SLANALYSE_BROKEN_PACKET})
         AND 
         (ServicelCounter.Latest < CallTimeStamp OR 
          ServiceLCounter.Latest = ?) THEN 
         ServicelCounter.Latest = CallTimeStamp.

      RELEASE ServiceLCounter.
   
   END.

   IF SlimitAllowed = {&SLANALYSE_NO_PACKET} THEN RETURN FALSE.
   ELSE                                           RETURN TRUE.

END.

FUNCTION fTempTableIsServiceLimitAllowed RETURNS LOG
   (INPUT  iiMSID         AS INT,
    INPUT  msseq          AS INT,
    INPUT  iicustnum      AS INT,
    INPUT  InvSeq         AS INT,
    INPUT  iislseq        AS INT,
    INPUT  iiInclUnit     AS INT,
    INPUT  idInclAmt      AS DEC,
    INPUT  CallTimeStamp  AS DEC,
    INPUT  iiDCType       AS INT,
    INPUT  iiBDestLimit   AS INT,
    INPUT  iiBDestAmt     AS INT,
    INPUT-OUTPUT  ideAmt  AS DEC).
                           
   DEF VAR liperiod       AS INT  NO-UNDO.
   DEF VAR SLimitAlloWed  AS INT  NO-UNDO.
   DEF VAR liQty          AS INT  NO-UNDO.
   DEF VAR ldeLimitAmt    AS DEC  NO-UNDO.
                                       
   ASSIGN 
      liQty    = 0 
      liPeriod = INT(substring(STRING(CallTimeStamp),1,6))
      SLimitAlloWed = {&SLANALYSE_NO_PACKET}.

   DO WHILE TRUE :
   
      IF iicustnum > 0 THEN
         FIND FIRST ttServiceLCounter WHERE
                    ttServiceLCounter.Custnum = iiCustnum AND
                    ttserviceLCounter.SLseq   = iiSlseq   AND 
                    ttServiceLCounter.Period  = liPeriod  AND 
                    ttServiceLCounter.MsId    = iiMSID    NO-ERROR.
      ELSE
         FIND FIRST ttServiceLCounter WHERE
                    ttServiceLCounter.Msseq   = msseq    AND
                    ttserviceLCounter.SLseq   = iiSlseq  AND 
                    ttServiceLCounter.Period  = liPeriod AND 
                    ttServiceLCounter.MsId    = iiMSID   NO-ERROR.

      /* NEW ServiceLimit Counter */
      IF NOT AVAIL ttServiceLCounter THEN DO:

         CREATE ttServiceLCounter.
         ASSIGN
            ttServiceLCounter.MSID = iiMSID
            ttServiceLCounter.InvSeq = InvSeq
            ttServiceLCounter.msseq  = (IF iiCustnum > 0 THEN 0 ELSE msseq)
            ttServiceLCounter.Custnum = iiCustnum
            ttServiceLCounter.slseq  = iiSlseq
            ttServiceLCounter.Period = liPeriod 
            ttServiceLCounter.amt    = 0
            ttServiceLCounter.Latest = 0
            SLimitAllowed            = {&SLANALYSE_FULL_PACKET} NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
         ELSE LEAVE.
      END.
      LEAVE.

   END.   

   /* update ServiceLimit counter */
   IF Avail ttServiceLCounter THEN DO:

      /* Populate InvSeq if it was not populated before for DSS */
      IF iicustnum > 0 AND
         (ttServiceLCounter.InvSeq = 0 OR ttServiceLCounter.InvSeq = ?) THEN
         ttServiceLCounter.InvSeq = InvSeq.
      
      IF iiDCType EQ 6 THEN DO:
         IF iicustnum > 0 THEN
            FIND FIRST MServiceLPool WHERE
                       MServiceLPool.Custnum = iiCustnum AND
                       MServiceLPool.SlSeq = iiSlseq AND
                       MserviceLPool.EndTS >= CallTimeStamp AND
                       MserviceLPool.fromTS <= CallTimeStamp NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST MServiceLPool WHERE
                       MServiceLPool.MsSeq = msseq AND
                       MServiceLPool.SlSeq = iiSlseq AND
                       MserviceLPool.EndTS >= CallTimeStamp AND
                       MserviceLPool.fromTS <= CallTimeStamp NO-LOCK NO-ERROR.

         IF AVAIL MServiceLPool THEN ldeLimitAmt = MServiceLPool.LimitAmt.
         ELSE RETURN FALSE.
      END.
      ELSE ldeLimitAmt = idInclAmt.

      ideAmt = fConvertAmountUnit(iiInclUnit,ideAmt).

      CASE iiInclUnit:
         WHEN {&INCLUNIT_MEGABYTE} THEN 
            ldeLimitAmt = ldeLimitAmt * 1024 * 1024.
         WHEN {&INCLUNIT_GIGABYTE} THEN
            ldeLimitAmt = ldeLimitAmt * 1024 * 1024 * 1024.
         WHEN {&INCLUNIT_MINUTE} THEN 
            ldeLimitAmt = ldeLimitAmt * 60.
      END.

      ttServiceLCounter.limit = ttServiceLCounter.limit + iiBDestAmt.
 
      IF ttServiceLCounter.amt < ldeLimitAmt AND
         ttServiceLCounter.limit <= iiBDestLimit THEN DO:

         ASSIGN
            ttServiceLCounter.amt = ttServiceLCounter.amt + ideAmt
            SLimitAllowed         = {&SLANALYSE_FULL_PACKET}.

         /* what is left after this */
         IF ttServiceLCounter.Amt > ldeLimitAmt THEN ASSIGN
            ideAmt = ttServiceLCounter.Amt - ldeLimitAmt
            ttServiceLCounter.Amt = ldeLimitAmt
            SLimitAllowed         = {&SLANALYSE_BROKEN_PACKET}.
         ELSE ideAmt = 0.   
      END.      
      ELSE SLimitAllowed = {&SLANALYSE_NO_PACKET}.

      /* The most likely to belongs to packet -> needs triggeractions  */
      IF (SLimitAllowed = {&SLANALYSE_NO_PACKET}     AND ttServicelCounter.Latest > CallTimeStamp)   OR  
         (SliMitAllowed = {&SLANALYSE_BROKEN_PACKET} AND ttServicelCounter.Latest > CallTimeStamp)  
      THEN DO:
         fTriggerEvent(CallTimeStamp,
                       ttServicelCounter.MSSeq,
                       ttServicelCounter.Latest).
      END.

      ELSE IF 
        (SLimitAllowed = {&SLANALYSE_FULL_PACKET} OR 
         SLimitAllowed = {&SLANALYSE_BROKEN_PACKET}) 
         AND 
         (ttServicelCounter.Latest < CallTimeStamp OR 
          ttServiceLCounter.Latest = ?) THEN 
         ttServicelCounter.Latest = CallTimeStamp.

   END.

   IF SLimitAllowed = {&SLANALYSE_NO_PACKET} THEN RETURN FALSE.
   ELSE                                           RETURN TRUE.

END.

FUNCTION fCheckSLCounterItem RETURNS INT
   (INPUT  piMsseq        AS INT,
    INPUT  piSLseq        AS INT,
    INPUT  ideCallTimeStamp AS DEC,
    INPUT  icGsmBnr       AS CHAR).
      
   DEF VAR liPeriod AS INT NO-UNDO. 
   liPeriod = INT(substring(STRING(ideCallTimeStamp),1,6)).
   
   IF CAN-FIND(FIRST SLCounterItem NO-LOCK WHERE
                     SLCounterItem.MsSeq = piMsSeq AND
                     SLCounterItem.Period = liperiod AND
                     SLCounterItem.SLSeq = piSLseq AND
                     SLCounterItem.SLCItem = icGsmBnr) THEN RETURN 0.
            
   CREATE SLCounterItem.
   ASSIGN
      SLCounterItem.MsSeq = piMsSeq
      SLCounterItem.Period = liperiod
      SLCounterItem.SLSeq = piSLseq 
      SLCounterItem.SLCItem = icGsmBnr.

   RETURN 1.

END.

FUNCTION fTempTableCheckSLCounterItem RETURNS INT
   (INPUT  piMsseq        AS INT,
    INPUT  piSLseq        AS INT,
    INPUT  ideCallTimeStamp AS DEC,
    INPUT  icGsmBnr       AS CHAR).
      
   DEF VAR liPeriod AS INT NO-UNDO. 
   liPeriod = INT(substring(STRING(ideCallTimeStamp),1,6)).
   
   FIND FIRST ttSLCounterItem EXCLUSIVE-LOCK WHERE
              ttSLCounterItem.MsSeq = piMsSeq AND
              ttSLCounterItem.Period = liperiod AND
              ttSLCounterItem.SLSeq = piSLseq AND
              ttSLCounterItem.SLCItem = icGsmBnr NO-ERROR.

   IF AVAIL ttSLCounterItem THEN DO:
     IF ttSLCounterItem.Picked > 0 THEN RETURN 0.
     ttSLCounterItem.Picked = 1.
     RETURN 1.
   END.
            
   CREATE ttSLCounterItem.
   ASSIGN
      ttSLCounterItem.MsSeq = piMsSeq
      ttSLCounterItem.Period = liperiod
      ttSLCounterItem.SLSeq = piSLseq 
      ttSLCounterItem.SLCItem = icGsmBnr
      ttSLCounterItem.Picked = 2.

   RETURN 1.

END.


FUNCTION fPCCQuery RETURN DECIMAL
   (INPUT  iiMSID         AS INT,
    INPUT  iimsseq        AS INT,
    INPUT  iiCustnum      AS INT,
    INPUT  iislseq        AS INT,
    INPUT  iiPeriod       AS INT).
    
   IF iiCustnum > 0 THEN
      FIND FIRST ServiceLCounter WHERE 
                 ServiceLCounter.Custnum = iiCustnum AND 
                 ServiceLCounter.SLSeq   = iislseq   AND 
                 ServiceLCounter.Period  = iiPeriod AND
                 ServiceLCounter.MSID    = iiMSID NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST ServiceLCounter WHERE 
                 ServiceLCounter.MSSeq   = iimsseq   AND 
                 ServiceLCounter.SLSeq   = iislseq   AND 
                 ServiceLCounter.Period  = iiPeriod  AND
                 ServiceLCounter.MSID    = iiMSID NO-LOCK NO-ERROR.
              
   IF AVAIL ServiceLCounter THEN RETURN ServicelCounter.Amt.           
   ELSE                          RETURN 0.

END.

FUNCTION fTempTablePCCQuery RETURN DECIMAL
   (INPUT  iiMSID         AS INT,
    INPUT  iimsseq        AS INT,
    INPUT  iiCustnum      AS INT,
    INPUT  iislseq        AS INT,
    INPUT  iiPeriod       AS INT).
    
   IF iiCustnum > 0 THEN
      FIND FIRST ttServiceLCounter WHERE 
                 ttServiceLCounter.Custnum = iiCustnum AND 
                 ttServiceLCounter.SLSeq   = iislseq   AND 
                 ttServiceLCounter.Period  = iiPeriod  AND
                 ttServiceLCounter.MSID    = iiMSID NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST ttServiceLCounter WHERE 
                 ttServiceLCounter.MSSeq   = iimsseq   AND 
                 ttServiceLCounter.SLSeq   = iislseq   AND 
                 ttServiceLCounter.Period  = iiPeriod  AND
                 ttServiceLCounter.MSID    = iiMSID NO-LOCK NO-ERROR.
              
   IF AVAIL ttServiceLCounter THEN RETURN ttServicelCounter.Amt.           
   ELSE RETURN 0.

END.

FUNCTION fProgLimitAnalyse RETURNS LOG
   (INPUT  iiMSID            AS INT,
    INPUT  iimsseq           AS INT,
    INPUT  iiCustnum         AS INT,
    INPUT  iislseq           AS INT,
    INPUT  iiInclUnit        AS INT,
    INPUT  ideCallTimeStamp  AS DEC,
    INPUT  ldeUpperLimit     AS DEC,
    INPUT  icContract        AS CHAR,
    INPUT  iiPeriod          AS INT,
    INPUT-OUTPUT  ideAmt     AS DEC).
                           
   DEF VAR SLimitAlloWed  AS INT  NO-UNDO.
   DEF VAR liQty          AS INT  NO-UNDO.
   DEF VAR ldLimitAmt     AS DEC NO-UNDO. 
   DEF VAR ldeCDRUpperLimit AS DEC NO-UNDO. 

   ASSIGN 
      ideAmt        = fConvertAmountUnit(iiInclUnit,ideAmt)  
      liQty         = 0 
      SLimitAlloWed = {&SLANALYSE_NO_PACKET}.

   COUNTER:
   DO WHILE TRUE :
      IF iiCustnum > 0 THEN
         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.Custnum = iiCustnum           AND
                    serviceLCounter.SLseq   = iiSlseq             AND 
                    ServiceLCounter.Period  = iiPeriod  AND
                    ServiceLCounter.MSID = iiMSID
              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      ELSE
         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.Msseq   = iimsseq             AND
                    serviceLCounter.SLseq   = iiSlseq             AND 
                    ServiceLCounter.Period  = iiPeriod  AND
                    ServiceLCounter.MSID = iiMSID
              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      liQty = liQty + 1.
      
      IF liQty > 30 THEN LEAVE.
       
      IF LOCKED(servicelcounter) THEN DO:
         PAUSE 1 NO-MESSAGE.
         NEXT.                             
      END.                       

      /* NEW ServiceLimit Counter */
      IF NOT AVAIL ServiceLCounter THEN DO:

         CREATE ServiceLCounter.
         ASSIGN
            ServiceLCounter.MSID   = iiMSID
            ServiceLCounter.msseq  = (IF iiCustnum > 0 THEN 0 ELSE iimsseq)
            ServiceLCounter.Custnum = iiCustnum
            ServiceLCounter.slseq  = iiSlseq
            ServiceLCounter.Period = iiPeriod 
            ServiceLCounter.amt    = 0
            ServiceLCounter.Latest = 0
            SLimitAllowed          = {&SLANALYSE_FULL_PACKET} NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
         ELSE LEAVE.
      
      END.
      LEAVE.
   END.   

   /* update ServiceLimit counter */
   IF Avail ServiceLCounter THEN DO:
      
      IF LOOKUP(icContract,{&DSS_BUNDLES}) > 0 THEN DO:
         
         FIND FIRST MServiceLPool WHERE
                    MServiceLPool.Custnum = iiCustnum AND
                    MServiceLPool.SlSeq = iiSlseq AND
                    MserviceLPool.EndTS >= ideCallTimeStamp AND
                    MserviceLPool.fromTS <= ideCallTimeStamp
         NO-LOCK NO-ERROR.

         IF AVAIL MserviceLPool THEN
            ldeCDRUpperLimit = (MServiceLPool.LimitAmt * 1024 * 1024).
         ELSE ldeCDRUpperLimit = ldeUpperLimit.
       
         IF ServiceLCounter.amt < ldeCDRUpperLimit THEN DO:
            ServiceLCounter.amt = ServiceLCounter.amt + ideAmt.

            IF ServiceLCounter.amt > ldeCDRUpperLimit THEN
               ASSIGN ideAmt =  (ServiceLCounter.amt - ldeCDRUpperLimit)
                      ServiceLCounter.Amt = ldeCDRUpperLimit.
            ELSE ideAmt = 0.
         END.

         SLimitAllowed = {&SLANALYSE_FULL_PACKET}.
      END.
      ELSE IF ServiceLCounter.amt < ldeUpperLimit THEN DO:
      
         ASSIGN
            ServiceLCounter.amt = ServiceLCounter.amt + ideAmt
            SLimitAllowed       = {&SLANALYSE_FULL_PACKET}.

         IF ServiceLCounter.amt > ldeUpperLimit THEN ASSIGN 
            ideAmt         =  ServiceLCounter.amt  - ldeUpperLimit
            ServiceLCounter.Amt = ldeUpperLimit
            SLimitAllowed       = {&SLANALYSE_BROKEN_PACKET} .
         ELSE ideAmt = 0.   
      END.   
                   
      ELSE SLimitAllowed = {&SLANALYSE_NO_PACKET}.
      
      IF (SLimitAllowed = {&SLANALYSE_NO_PACKET}     AND ServicelCounter.Latest > ideCallTimeStamp)   OR 
         (SLimitAllowed = {&SLANALYSE_BROKEN_PACKET} AND ServicelCounter.Latest > ideCallTimeStamp) THEN DO:

         fTriggerEvent(INPUT ideCallTimeStamp,
                       ServicelCounter.MSSeq, 
                       ServicelCounter.Latest).
      END.
      ELSE IF 
         (SLimitAllowed = {&SLANALYSE_FULL_PACKET} OR
          SLimitAllowed = {&SLANALYSE_BROKEN_PACKET}) 
          AND 
         (ServicelCounter.Latest < ideCallTimeStamp OR 
          ServiceLCounter.Latest = ?) THEN 
         ServicelCounter.Latest = ideCallTimeStamp.

      RELEASE ServiceLCounter.
   END.
   
   IF SlimitAllowed = {&SLANALYSE_NO_PACKET} THEN RETURN FALSE.
   ELSE                                           RETURN TRUE .

END.      

FUNCTION fTempTableProgLimitAnalyse RETURNS LOG
   (INPUT  iiMSID            AS INT,
    INPUT  iiMsSeq           AS INT,
    INPUT  iiCustnum         AS INT,
    INPUT  iiSLSeq           AS INT,
    INPUT  iiInclUnit        AS INT,
    INPUT  ideCallTimeStamp  AS DEC,
    INPUT  ldeUpperLimit     AS DEC,
    INPUT  icContract        AS CHAR,
    INPUT  iiPeriod          AS INT,
    INPUT-OUTPUT  ideAmt     AS DEC):
                           
   DEF VAR SLimitAlloWed    AS INT  NO-UNDO.
   DEF VAR ldeCDRUpperLimit AS DEC NO-UNDO. 

   ASSIGN 
      ideAmt        = fConvertAmountUnit(iiInclUnit,ideAmt)
      SLimitAlloWed = {&SLANALYSE_NO_PACKET}.

   IF iiCustnum > 0 THEN
      FIND FIRST ttServiceLCounter WHERE
                 ttServiceLCounter.Custnum = iiCustnum AND
                 ttserviceLCounter.SLSeq   = iiSLSeq   AND 
                 ttServiceLCounter.Period =  iiPeriod  AND
                 ttServiceLCounter.MSID = iiMsid NO-ERROR.
   ELSE
      FIND FIRST ttServiceLCounter WHERE
                 ttServiceLCounter.MsSeq  = iiMsSeq AND
                 ttserviceLCounter.SLSeq  = iiSLSeq AND 
                 ttServiceLCounter.Period = iiPeriod AND
                 ttServiceLCounter.MSID = iiMsid NO-ERROR.

   /* NEW ServiceLimit Counter */
   IF NOT AVAIL ttServiceLCounter THEN DO TRANS:

      CREATE ttServiceLCounter.
      ASSIGN
         ttServiceLCounter.MSID  = iiMSID
         ttServiceLCounter.MsSeq  = (IF iiCustnum > 0 THEN 0 ELSE iimsseq)
         ttServiceLCounter.Custnum = iiCustnum
         ttServiceLCounter.SLSeq  = iiSLSeq
         ttServiceLCounter.Period = iiPeriod 
         ttServiceLCounter.Amt    = 0
         ttServiceLCounter.Latest = 0
         SLimitAllowed            = {&SLANALYSE_FULL_PACKET}.
   END.   

   /* update ServiceLimit counter */
   IF Avail ttServiceLCounter THEN DO:
      
      IF LOOKUP(icContract,{&DSS_BUNDLES}) > 0 THEN DO:
         
         FIND FIRST MServiceLPool WHERE
                    MServiceLPool.Custnum = iiCustnum AND
                    MServiceLPool.SlSeq = iiSlseq AND
                    MserviceLPool.EndTS >= ideCallTimeStamp AND
                    MserviceLPool.fromTS <= ideCallTimeStamp
         NO-LOCK NO-ERROR.

         IF AVAIL MserviceLPool THEN
            ldeCDRUpperLimit = (MServiceLPool.LimitAmt * 1024 * 1024).
         ELSE ldeCDRUpperLimit = ldeUpperLimit.
       
         IF ttServiceLCounter.amt < ldeCDRUpperLimit THEN DO:
            ttServiceLCounter.amt = ttServiceLCounter.amt + ideAmt.

            IF ttServiceLCounter.amt > ldeCDRUpperLimit THEN
               ASSIGN ideAmt =  (ttServiceLCounter.amt - ldeCDRUpperLimit)
                      ttServiceLCounter.Amt = ldeCDRUpperLimit.
            ELSE ideAmt = 0.
         END.

         SLimitAllowed = {&SLANALYSE_FULL_PACKET}.
      END.
      
      ELSE IF ttServiceLCounter.Amt < ldeUpperLimit     THEN DO:

         ASSIGN
            ttServiceLCounter.Amt = ttServiceLCounter.Amt + ideAmt
            SLimitAllowed         = {&SLANALYSE_FULL_PACKET}.

         IF ttServiceLCounter.amt > ldeUpperLimit THEN ASSIGN 
            ideAmt                =  ttServiceLCounter.amt  - ldeUpperLimit
            SLimitAllowed         = {&SLANALYSE_BROKEN_PACKET} 
            ttServiceLCounter.Amt = ldeUpperLimit.
         ELSE ideAmt = 0.   
      END.   
                   
      ELSE SLimitAllowed = {&SLANALYSE_NO_PACKET}.
      
      IF (SLimitAllowed = {&SLANALYSE_NO_PACKET}     AND ttServicelCounter.Latest > ideCallTimeStamp) OR 
         (SLimitAllowed = {&SLANALYSE_BROKEN_PACKET} AND ttServicelCounter.Latest > ideCallTimeStamp)
      THEN DO:
         fTriggerEvent(INPUT ideCallTimeStamp,
                       ttServicelCounter.MSSeq, 
                       ttServicelCounter.Latest).
      END.
      ELSE IF 
         (SLimitAllowed  =  {&SLANALYSE_FULL_PACKET} OR
          SLimitAllowed = {&SLANALYSE_BROKEN_PACKET}) 
          AND 
         (ttServicelCounter.Latest < ideCallTimeStamp OR 
          ttServiceLCounter.Latest = ?) THEN 
          ttServicelCounter.Latest = ideCallTimeStamp.
   END.
   
   IF SliMItAllowed = {&SLANALYSE_NO_PACKET} THEN RETURN FALSE.
   ELSE                                           RETURN TRUE.

END.

FUNCTION fServiceLCounter2Temp RETURNS LOGICAL
   (INPUT iiMsSeq     AS INT,
    INPUT iiCustnum   AS INT,
    INPUT idaFromDate AS DATE,
    INPUT idaToDate   AS DATE):

   DEF VAR liPeriodFrom AS INT NO-UNDO. 
   DEF VAR liPeriodTo AS INT NO-UNDO. 
   DEF VAR ldeServiceLCounterAmt AS DEC NO-UNDO.
   DEF VAR liPeriodType AS INT NO-UNDO.

   DO liPeriodType = 1 to 2:

      IF liPeriodType = 1 THEN ASSIGN
         liPeriodFrom = YEAR(idaFromDate) * 100 + MONTH(idaFromDate)
         liPeriodTo   = YEAR(idaToDate) * 100 + MONTH(idaToDate).
      ELSE ASSIGN
         liPeriodFrom = (liPeriodFrom * 100) + 1
         liPeriodTo   = (liPeriodTo * 100) + 31.

      FOR EACH ServiceLCounter NO-LOCK WHERE
               ServiceLCounter.MsSeq   = iiMsSeq AND
               ServiceLCounter.Period >= liPeriodFrom AND
               ServiceLcounter.Period <= liPeriodTo:

         IF CAN-FIND(FIRST ttServiceLCounter WHERE
                    ttServiceLCounter.MSSeq   = ServiceLCounter.MSSeq   AND
                    ttServiceLCounter.SLSeq   = ServiceLCounter.SLSeq   AND
                    ttServiceLCounter.Period  = ServiceLCounter.Period)
         THEN NEXT.

         CREATE ttServiceLCounter.
         BUFFER-COPY ServiceLCounter TO ttServiceLCounter.
         ASSIGN
         ttServiceLCounter.Amt    = 0
         ttServiceLCounter.Latest = 0
         ttServiceLCounter.Limit  = 0.

         FOR EACH SLCounterItem NO-LOCK WHERE
                  SLCounterItem.MsSeq   = ServiceLCounter.MsSeq AND
                  SLCounterItem.Period = ServiceLCounter.Period AND
                  SLCounterItem.SLSeq = ServiceLCounter.SLSeq:

            IF CAN-FIND(FIRST ttSLCounterItem WHERE
                       ttSLCounterItem.MSSeq   = SLCounterItem.MSSeq   AND
                       ttSLCounterItem.SLSeq   = SLCounterItem.SLSeq   AND
                       ttSLCounterItem.Period  = SLCounterItem.Period AND
                       ttSLCounterItem.SLCItem = SLCounterItem.SLCItem)
            THEN NEXT.

            CREATE ttSLCounterItem.
            BUFFER-COPY SLCounterItem TO ttSLCounterItem.
            ASSIGN
               ttSLCounterItem.Picked = 0.
         END.
      END.
   
      IF liPeriodType EQ 1 THEN
      FOR EACH ServiceLCounter NO-LOCK WHERE
               ServiceLCounter.Custnum = iiCustnum AND 
               ServiceLCounter.Period >= liPeriodFrom AND 
               ServiceLcounter.Period <= liPeriodTo:

         IF CAN-FIND(FIRST ttServiceLCounter WHERE
                    ttServiceLCounter.CustNum = ServiceLCounter.CustNum AND 
                    ttServiceLCounter.SLSeq   = ServiceLCounter.SLSeq   AND
                    ttServiceLCounter.Period  = ServiceLCounter.Period)
         THEN NEXT. 

         CREATE ttServiceLCounter.
         BUFFER-COPY ServiceLCounter TO ttServiceLCounter.
         ASSIGN ttServiceLCounter.Latest =  0
                ttServiceLCounter.Amt = 0
                ttServiceLCounter.Limit = 0.

         /* Get other bundle usages */
         ASSIGN
            ldeServiceLCounterAmt = fGetOtherBundleUsages(ServiceLCounter.Custnum,
                                                          ServiceLCounter.Period)
            ttServiceLCounter.Amt = (ldeServiceLCounterAmt * 1024 * 1024).
      END.
    
   END.

END FUNCTION.    

FUNCTION fTemp2ServiceLCounter RETURNS LOGICAL:

   DEF BUFFER bDblCounter FOR ServiceLCounter.

   FOR EACH ttServiceLCounter:
   
      GetServiceLCounter:
      REPEAT:
         IF ttServiceLCounter.Custnum > 0 THEN
            FIND FIRST ServiceLCounter WHERE
                       ServiceLCounter.Custnum = ttServiceLCounter.Custnum AND 
                       ServiceLCounter.Period  = ttServiceLCounter.Period  AND
                       ServiceLCounter.SLSeq   = ttServiceLCounter.SLSeq   AND
                       ServiceLCounter.MSID    = ttServiceLCounter.MSID
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         ELSE 
            FIND FIRST ServiceLCounter WHERE
                       ServiceLCounter.MsSeq  = ttServiceLCounter.MsSeq AND 
                       ServiceLCounter.Period = ttServiceLCounter.Period  AND
                       ServiceLCounter.SLSeq  = ttServiceLCounter.SLSeq   AND
                       ServiceLCounter.MSID   = ttServiceLCounter.MSID
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

         IF LOCKED(ServiceLCounter) THEN DO:
            PAUSE 2 NO-MESSAGE.
            NEXT GetServiceLCounter.
         END.
         ELSE LEAVE GetServiceLCounter.
      END.   

      IF NOT AVAILABLE ServiceLCounter THEN CREATE ServiceLCounter.
      ELSE DO:
         /* remove possible double counter */
         IF ttServiceLCounter.Custnum > 0 THEN
            FIND FIRST bDblCounter NO-LOCK WHERE
                       bDblCounter.Custnum = ttServiceLCounter.Custnum AND 
                       bDblCounter.Period  = ttServiceLCounter.Period AND
                       bDblCounter.SLSeq   = ttServiceLCounter.SLSeq   AND
                       bDblCounter.MSID    = ttServiceLCounter.MSID AND
                       ROWID(bDblCounter) NE ROWID(ServiceLCounter) NO-ERROR.
         ELSE
            FIND FIRST bDblCounter NO-LOCK WHERE
                       bDblCounter.MSSeq   = ttServiceLCounter.MSSeq   AND
                       bDblCounter.Period  = ttServiceLCounter.Period AND
                       bDblCounter.SLSeq   = ttServiceLCounter.SLSeq   AND
                       bDblCounter.MSID    = ttServiceLCounter.MSID AND
                       ROWID(bDblCounter) NE ROWID(ServiceLCounter) NO-ERROR.

         IF AVAILABLE bDblCounter THEN DO:
            FIND CURRENT bDblCounter EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF AVAILABLE bDblCounter THEN DELETE bDblCounter.
         END.
      END.    

      BUFFER-COPY ttServiceLCounter TO ServiceLCounter.

      RELEASE ServiceLCounter.
   
      FOR EACH ttSLCounterItem WHERE
               ttSLCounterItem.MsSeq = ttServiceLCounter.MsSeq AND
               ttSLCounterItem.Period = ttServiceLCounter.Period AND
               ttSLCounterItem.SlSeq = ttServiceLCounter.SLSeq:

         IF ttSLCounterItem.Picked EQ 0 THEN DO:
            GetSLCounterItem:
            REPEAT:
               FIND FIRST SLCounterItem WHERE
                          SLCounterItem.MSSeq   = ttSLCounterItem.MSSeq   AND
                          SLCounterItem.SLSeq   = ttSLCounterItem.SLSeq   AND
                          SLCounterItem.Period  = ttSLCounterItem.Period  AND
                          SLCounterItem.SLCItem = ttSLCounterItem.SLCItem
               EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

               IF LOCKED(SLCounterItem) THEN DO:
                  PAUSE 2 NO-MESSAGE.
                  NEXT GetSLCounterItem.
               END.
               ELSE LEAVE GetSLCounterItem.
            END.   
            
            IF AVAIL SLCounterItem THEN DELETE SLCounterItem.
         END.
         ELSE IF ttSLCounterItem.Picked EQ 2 THEN DO:

            IF NOT CAN-FIND(FIRST SLCounterItem NO-LOCK WHERE
                                  SLCounterItem.MSSeq  = ttSLCounterItem.MSSeq AND
                                  SLCounterItem.SLSeq  = ttSLCounterItem.SLSeq AND
                                  SLCounterItem.Period = ttSLCounterItem.Period AND
                                  SLCounterItem.SLCItem = ttSLCounterItem.SLCItem)
            THEN DO:

               CREATE SLCounterItem.
               BUFFER-COPY ttSLCounterItem TO SLCounterItem.
               RELEASE SLCounterItem.
            END.
         END.
      END.
   
   END.
   
   EMPTY TEMP-TABLE ttServiceLCounter.
   
   EMPTY TEMP-TABLE ttSLCounterItem.
   
   RETURN TRUE. 

END FUNCTION.


