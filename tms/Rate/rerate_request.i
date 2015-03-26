/* rerate_request.i         10.03.11/aam 

   create a rerate request 
*/

&IF "{&RERATE_REQUEST_I}" NE "YES"
&THEN

&GLOBAL-DEFINE RERATE_REQUEST_I YES
   
{fcreatereq.i}

FUNCTION fRerateRequest RETURNS INTEGER
   (INPUT  iiInvCust     AS INT,  
    INPUT  iiMsSeq       AS INT,
    INPUT  idaFromDate   AS DATE,
    INPUT  idaToDate     AS DATE, 
    INPUT  ilWaitOthers  AS LOG,
    INPUT  ilDoubleCheck AS LOG, 
    INPUT  idActStamp    AS DEC,   
    INPUT  icCreator     AS CHAR,
    INPUT  icSource      AS CHAR,
    INPUT  iiOrigRequest AS INT,
    INPUT  iiMandatory   AS INT,
    OUTPUT ocResult      AS CHAR):

   DEF VAR liReqCreated AS INT  NO-UNDO.
   DEF VAR lcCLI        AS CHAR NO-UNDO.

   DEF BUFFER bReqOwner FOR MsOwner.
   
   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().

   IF iiMsSeq > 0 THEN DO:
      FIND FIRST bReqSub WHERE bReqSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bReqSub THEN DO:
         FIND FIRST bReqOwner WHERE bReqOwner.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bReqOwner THEN DO:
            ocResult = "Subscription not found".
            RETURN 0.
         END.
         lcCLI = bReqOwner.CLI.
      END.
      ELSE lcCLI = bReqSub.CLI.
   END.

   fCreateRequest(65,
                  idActStamp,
                  icCreator,
                  FALSE,   
                  FALSE).

   ASSIGN 
      bCreaReq.CustNum     = iiInvCust
      bCreaReq.MsSeq       = iiMsSeq
      bCreaReq.CLI         = lcCLI
      bCreaReq.ReqIParam1  = INTEGER(ilWaitOthers)
      bCreaReq.ReqIParam2  = INTEGER(ilDoubleCheck)
      bCreaReq.ReqDtParam1 = idaFromDate
      bCreaReq.ReqDtParam2 = idaToDate
      bCreaReq.OrigRequest = iiOrigRequest
      bCreaReq.Mandatory   = iiMandatory
      bCreaReq.ReqSource   = icSource
      liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

&ENDIF
