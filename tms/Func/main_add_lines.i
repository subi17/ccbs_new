&IF "{&MAIN_ADD_LINES_I}" NE "YES"
&THEN
&GLOBAL-DEFINE MAIN_ADD_LINES_I YES

FUNCTION fIsMainLineSubActive RETURNS LOGICAL
   (INPUT pcIdType AS CHAR,
    INPUT pcPersonId AS CHAR):

   DEF BUFFER Customer FOR Customer.
   DEF BUFFER Mobsub FOR Mobsub.

   FOR FIRST Customer WHERE
             Customer.Brand = Syst.Var:gcBrand AND
             Customer.OrgId = pcPersonId AND
             Customer.CustidType = pcIdType AND
             Customer.Roles NE "inactive" NO-LOCK,
       EACH  MobSub WHERE
             MobSub.Brand   = Syst.Var:gcBrand AND
             MobSub.InvCust = Customer.CustNum AND
             MobSub.PayType = FALSE NO-LOCK:

      IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                        CLIType.Brand = Syst.Var:gcBrand AND
                        CLIType.CLIType = MobSub.TariffBundle AND
                        CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN 
         RETURN TRUE.
   END.

   RETURN FALSE.
END.

FUNCTION fHasPendingSTCToNonMainLine RETURNS LOGICAL
   (INPUT iiMsSeq AS INT,
    INPUT ideActStamp AS DEC):

   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER CLIType FOR CLIType.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0 AND
            MsRequest.Actstamp <= ideActStamp,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = (IF MsRequest.ReqCParam5 > ""
                               THEN MsRequest.ReqCParam5
                               ELSE MsRequest.ReqCParam2):
      IF CLIType.LineType NE {&CLITYPE_LINETYPE_MAIN}
         THEN RETURN TRUE. 
   END.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0 AND
            MsRequest.Actstamp <= ideActStamp,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = MsRequest.ReqCParam2:

      IF CLIType.LineType NE {&CLITYPE_LINETYPE_MAIN} THEN RETURN TRUE. 
   END.

   RETURN FALSE.
END.

FUNCTION fHasPendingRequests RETURNS LOGICAL
   (INPUT iiMsSeq AS INT,
    INPUT icCLI AS CHAR,
    INPUT iiLineType AS INT):

   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER CLIType FOR CLIType.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = (IF MsRequest.ReqCParam5 > ""
                               THEN MsRequest.ReqCParam5
                               ELSE MsRequest.ReqCParam2):
      IF CLIType.LineType NE iiLineType 
         THEN RETURN TRUE. 
   END.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = MsRequest.ReqCParam2:

      IF CLIType.LineType NE iiLineType THEN RETURN TRUE. 
   END.
   
   IF CAN-FIND (FIRST MsRequest WHERE
          MsRequest.MsSeq = iiMsseq AND
          MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
          LOOKUP(STRING(MsRequest.ReqStatus),
                 {&REQ_INACTIVE_STATUSES}) = 0) THEN RETURN TRUE.

   IF Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT icCLI) THEN RETURN TRUE.

   RETURN FALSE.

END FUNCTION.

FUNCTION fHasPendingSTCToMainLine RETURNS LOGICAL
   (INPUT iiMsSeq AS INT):
   
   DEF BUFFER CLIType FOR CLIType.
   DEF BUFFER MsRequest FOR Msrequest.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = (IF MsRequest.ReqCParam5 > ""
                               THEN MsRequest.ReqCParam5
                               ELSE MsRequest.ReqCParam2):
      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} THEN RETURN TRUE. 
   END.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = MsRequest.ReqCParam2:

      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} THEN RETURN TRUE. 
   END.

   RETURN FALSE.

END.

FUNCTION fHasPendingSTCToNonAddLine RETURNS LOGICAL
   (INPUT iiMsSeq AS INT):

   DEF BUFFER CLIType FOR CLIType.
   DEF BUFFER MsRequest FOR Msrequest.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = (IF MsRequest.ReqCParam5 > ""
                               THEN MsRequest.ReqCParam5
                               ELSE MsRequest.ReqCParam2):
      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN}    OR 
         CLIType.LineType EQ {&CLITYPE_LINETYPE_NONMAIN} THEN RETURN TRUE.
   END.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = iiMsseq AND
            MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
            LOOKUP(STRING(MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = MsRequest.ReqCParam2:

      IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN}    OR 
         CLIType.LineType EQ {&CLITYPE_LINETYPE_NONMAIN} THEN RETURN TRUE.
   END.

   RETURN FALSE.

END.

&ENDIF
