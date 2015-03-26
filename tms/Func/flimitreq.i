&IF "{&flimitreq}" NE "YES"
&THEN

&GLOBAL-DEFINE flimitreq YES

{commali.i}
{fcreatereq.i}
{eventval.i}

DEF BUFFER bufLimit FOR Limit.
/* somehow have to prevent calling fCleanEventObjects */
DEF VAR llCleanFLimitReqEventLog AS LOGICAL NO-UNDO INIT TRUE.

IF llDoEvent THEN DO:

   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}

   DEFINE VARIABLE lhBufLimit     AS HANDLE    NO-UNDO.
   lhBufLimit = BUFFER bufLimit:HANDLE.
                                    
END.

DEF BUFFER bLimitCustomer FOR Customer.

FUNCTION fLimitRequest RETURNS INTEGER
   (INPUT  iiMsSeq        AS INT,    /* subscription     */
    INPUT  iiCustnum      AS INT,    /* customer         */
    INPUT  idActStamp     AS DEC,    /* when request should be handled */
    INPUT  icOperation    AS CHAR,   /* when request should be handled */
    INPUT  ideNewValue    AS DEC extent 2,    /* new limit value */
    INPUT  ilDefValue     AS LOG,    /* tmrule id */
    INPUT  iiTMRuleSeq    AS INT,    /* tmrule id */
    INPUT  iiLimitType    AS INT,    /* limit type */
    INPUT  icSource       AS CHAR,   /* source of request */
    OUTPUT ocResult       AS CHAR):

   DEF VAR liReqCreated AS INT NO-UNDO.
   
   /* Check ongoing limit requests */
   FIND FIRST MsRequest WHERE
      MsRequest.Brand = gcBrand AND
      MsRequest.Reqtype = 40 AND
      MsRequest.CustNum = iiCustnum AND
      LOOKUP(STRING(MsRequest.ReqStatus),"0,1,3") >  0 
   NO-LOCK NO-ERROR.

   IF AVAIL MsRequest THEN DO:
      ocResult = "Request creation not allowed due to business rules".
      RETURN 0.
   END.
   
   /* Check ongoing agreement customer requests */
   FIND FIRST MsRequest WHERE
      MsRequest.Brand = gcBrand AND
      MsRequest.Reqtype = 10 AND
      MsRequest.CustNum = iiCustnum AND
      LOOKUP(STRING(MsRequest.ReqStatus),"0,1,3,7") >  0 
   NO-LOCK NO-ERROR.

   IF AVAIL MsRequest THEN DO:
      ocResult = "Request creation not allowed due to business rules".
      RETURN 0.
   END.

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().

   fCreateRequest(40,
                  idActStamp,
                  "",
                  FALSE,    /* create fees */
                  FALSE).   /* sms */
   
   ASSIGN
      bCreaReq.CustNum     = iiCustNum
      bCreaReq.ReqCParam1  = icOperation
      bCreaReq.ReqDParam1  = ideNewValue[1]
      bCreaReq.ReqDParam2  = ideNewValue[2]
      bCreaReq.ReqIParam1  = iiLimitType
      bCreaReq.ReqIParam2  = iiTMRuleSeq
      bCreaReq.ReqIParam3  = INT(ilDefValue)
      bCreaReq.ReqSource   = icSource
      liReqCreated         = bCreaReq.MsRequest.
 
   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

FUNCTION fGetLimit RETURNS LOGICAL (
   INPUT iiCustnum AS INT,
   INPUT iiMsSeq AS INT,
   INPUT iiLimitType AS INT,
   INPUT iiLimitId AS INT,
   INPUT iiTMRuleSeq AS INT,
   INPUT idaDate AS DATE):
  
   IF iiMsSeq > 0 THEN /* Subscription related limit */
      FIND FIRST Limit WHERE
         Limit.MsSeq       = iiMsSeq AND
         Limit.LimitType   = iiLimitType AND
         Limit.TMRuleSeq   = iiTMRuleSeq AND
         Limit.LimitId     = iiLimitID AND
         Limit.ToDate     >= idaDate AND
         Limit.Custnum     = iiCustnum NO-LOCK NO-ERROR. 
   ELSE /* Customer related limit */ 
      FIND FIRST Limit WHERE
         Limit.Custnum     = iiCustnum AND
         Limit.LimitType   = iiLimitType AND
         Limit.TMRuleSeq   = iiTMRuleSeq AND
         Limit.LimitId     = iiLimitID AND
         Limit.ToDate     >= idaDate NO-LOCK NO-ERROR. 

   IF NOT AVAIL Limit THEN RETURN FALSE.
   
   RETURN TRUE.

END.

FUNCTION fSetLimit RETURNS LOGICAL 
   (INPUT irRecord AS ROWID,
    INPUT ideNewLimit AS DEC,
    INPUT ilDefValue AS LOG,
    INPUT idaValidFrom AS DATE,
    INPUT idaValidTo AS DATE):
  
   FIND bufLimit EXCLUSIVE-LOCK WHERE
      ROWID(bufLimit) = irRecord.

   IF llDoEvent THEN DO:
      RUN StarEventInitialize(lhBufLimit).
   END.
   
   IF llDoEvent THEN RUN StarEventSetOldBuffer ( lhBufLimit ).
   
   ASSIGN
      bufLimit.LimitAmt = ideNewLimit
      bufLimit.DefValue = ilDefValue
      bufLimit.FromDate = idaValidFrom
      bufLimit.ToDate = idaValidTo.
   
   IF llDoEvent THEN RUN StarEventMakeModifyEvent ( lhBufLimit ).
   
   IF llCleanFLimitReqEventLog THEN fCleanEventObjects().

   FIND CURRENT bufLimit NO-LOCK.

   RETURN TRUE.

END.

FUNCTION fCreateLimit RETURNS LOGICAL
  (INPUT iiCustnum AS INT,
   INPUT iiMsSeq AS INT,
   INPUT iiLimitType AS INT,
   INPUT ideLimit AS DEC,
   INPUT iiLimitId AS INT,
   INPUT iiTMRuleSeq AS INT,
   INPUT idaValidFrom AS DATE,
   INPUT idaValidTo AS DATE):
   
   IF llDoEvent THEN DO:
      RUN StarEventInitialize( lhBufLimit ).
   END.
   
   CREATE bufLimit.
   ASSIGN
      bufLimit.Custnum   = iiCustnum
      bufLimit.MsSeq     = iiMsSeq
      bufLimit.DefValue  = FALSE 
      bufLimit.FromDate  = idaValidFrom 
      bufLimit.LimitAmt  = ideLimit
      bufLimit.LimitId   = iiLimitId
      bufLimit.ValueType = 1 
      bufLimit.TMRuleSeq = iiTMRuleSeq
      bufLimit.LimitType = iiLimitType
      bufLimit.ToDate    = idaValidTo.
   
   IF llDoEvent THEN DO:
      RUN StarEventMakeCreateEvent( lhBufLimit ).
      IF llCleanFLimitReqEventLog THEN fCleanEventObjects().
   END.

   RELEASE bufLimit.

END.

FUNCTION fCreateLimitHistory RETURNS LOGICAL
  (INPUT iiCustnum AS INT,
   INPUT iiMsSeq AS INT,
   INPUT iiLimitType AS INT,
   INPUT ideLimit AS DEC,
   INPUT iiLimitId AS INT,
   INPUT iiTMRuleSeq AS INT,
   INPUT ilDefValue AS LOG,
   INPUT idaValidFrom AS DATE,
   INPUT idaValidTo AS DATE):
   
   fGetLimit (iiCustnum, iiMsSeq, iiLimitType, iiLimitId, iiTMRuleSeq, TODAY).
   
   DO TRANS: 
      
      IF AVAIL Limit THEN DO:
         
         IF Limit.LimitAmt = ideLimit AND
            Limit.ToDate   = idaValidTo THEN RETURN TRUE.
         
         IF Limit.FromDate < TODAY AND 
            Limit.ToDate >= TODAY THEN DO:
            
            fSetLimit(
               ROWID(Limit),
               Limit.LimitAmt,
               Limit.DefValue,
               Limit.FromDate,
               idaValidFrom - 1).
         END.
         ELSE IF Limit.FromDate = idaValidFrom THEN DO:
            fSetLimit(
               ROWID(Limit),
               ideLimit,
               ilDefValue,
               Limit.FromDate,
               idaValidTo 
               ).
            RETURN TRUE.
         END.
      END.

      IF llDoEvent THEN DO:
         RUN StarEventInitialize( lhBufLimit ).
      END.
      
      CREATE bufLimit.
      ASSIGN
         bufLimit.Custnum   = iiCustnum
         bufLimit.MsSeq     = iiMsSeq
         bufLimit.DefValue  = ilDefValue 
         bufLimit.FromDate  = idaValidFrom 
         bufLimit.LimitAmt  = ideLimit
         bufLimit.LimitId   = iiLimitId
         bufLimit.TMRuleSeq = iiTMRuleSeq
         bufLimit.ValueType = 1 
         bufLimit.LimitType = iiLimitType
         bufLimit.ToDate    = idaValidTo.
      
      IF llDoEvent THEN DO:
         RUN StarEventMakeCreateEvent( lhBufLimit ).
         IF llCleanFLimitReqEventLog THEN fCleanEventObjects().
      END.

      RELEASE bufLimit.
   END.
END.

&ENDIF
