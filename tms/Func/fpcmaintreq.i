/* fpcmaintreq.i         17.03.08/aam 

   create a request for periodical contract maintenance 
*/
&IF "{&FPCMAINTREQ_I}" NE "YES"
&THEN

&GLOBAL-DEFINE FPCMAINTREQ_I YES
   
{Syst/commali.i}
{Func/fcreatereq.i}

FUNCTION fPCMaintenanceRequest RETURNS INTEGER
   (INPUT  iiMsSeq      AS INT,    /* subscription         */
    INPUT  icContrType  AS CHAR,   /* daycampaign.dcevent  */
    INPUT  icFields     AS CHAR,   /* field list (;)       */
    INPUT  icValues     AS CHAR,   /* list of new values (;) */
    INPUT  idActStamp   AS DEC,    /* when request should be handled */
    INPUT  ilCreateFees AS LOG,    /* fees */
    INPUT  icSource     AS CHAR,   /* where created */
    INPUT  icCreator    AS CHAR,   /* who made the request */
    INPUT  iiOrigRequest AS INT,   /* main request */
    INPUT  ilMandatory  AS LOG,    /* main request waits for this */
    OUTPUT ocResult     AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF BUFFER bReqOwner FOR MsOwner.

   ocResult = fChkRequest(iiMsSeq,
                          8,
                          icContrType,
                          icCreator).

   IF ocResult > "" THEN RETURN 0.            

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().

   fCreateRequest(8,
                  idActStamp,
                  icCreator,
                  ilCreateFees, 
                  FALSE).   /* sms */

   IF NOT AVAILABLE bReqSub THEN DO:
      FIND FIRST bReqOwner WHERE bReqOwner.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bReqOwner THEN DO:
         DELETE bCreaReq.
         ocResult = "Subscription not found".
         RETURN 0.
      END.

      ASSIGN 
          bCreaReq.MsSeq      = bReqOwner.MsSeq
          bCreaReq.CLI        = bReqOwner.CLI
          bCreaReq.CustNum    = bReqOwner.CustNum.
   END.
   
   ASSIGN bCreaReq.ReqSource   = icSource
          bCreaReq.ReqCParam1  = icFields
          bCreaReq.ReqCParam2  = "update"
          bCreaReq.ReqCParam3  = icContrType
          bCreaReq.ReqCParam4  = icValues
          bCreaReq.CreateFees  = ilCreateFees
          bCreaReq.OrigRequest = iiOrigRequest
          bCreaReq.Mandatory   = INTEGER(ilMandatory)
          liReqCreated         = bCreaReq.MsRequest.
 
   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

&ENDIF
