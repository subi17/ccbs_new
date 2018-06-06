/*-----------------------------------------------------------------------------
  MODULE .......: msreqfunc.i
  FUNCTION .....: General functions for msrequest handling
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 13.03.06 (separated from msrequest.i)
  CHANGED.. ....: 29.11.06/aam fMakeServiceSolog returns integer
                  24.10.07/jp fChkSubRequest analyses
                  20.11.07/jt mark MsRequest.UpdateStamp when
                              status is changed
  Version ......: M15
  -------------------------------------------------------------------------- */

&IF "{&msreqfunc}" NE "YES"
&THEN

&GLOBAL-DEFINE msreqfunc YES
{Func/cparam2.i}

DEF STREAM sReqLog.

DEF VAR lcEventDir   AS CHAR NO-UNDO.
DEF VAR ldtActDate   AS DATE NO-UNDO.
DEF VAR liActTime    AS INT  NO-UNDO. 
DEF VAR llAllowed    AS LOG  NO-UNDO. 
DEF VAR liReqCnt     AS INT  NO-UNDO. 
DEF VAR ldReqAmt     AS DEC  NO-UNDO. 
DEF VAR lcReqChar    AS CHAR NO-UNDO. 
DEF VAR lcSMSText    AS CHAR NO-UNDO.
DEF VAR ldCurrStamp  AS DEC  NO-UNDO. 
DEF VAR ldReqStamp   AS DEC  NO-UNDO. 
DEF VAR llReRate     AS LOG  NO-UNDO. 
DEF VAR lcReqType    AS CHAR NO-UNDO. 

FUNCTION  fReRateTriggerEvent RETURN LOGICAL
(INPUT iiRequestID  AS INT,
 INPUT iiRecid      AS RECID):

   CREATE TriggerEvent.
   ASSIGN
      TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
      TriggerEvent.TriggerConfID  = "MSREquest"
      TriggerEvent.EventSource    = "MODIFY"
      TriggerEvent.Created        = DateTime(Today,mtime)
      TriggerEvent.TableID        = iiRecID
      TriggerEvent.TableName      = "MSRequest"
      TriggerEvent.Keyvalue       = STRING(iiRequestID).

END FUNCTION.

FUNCTION fChkSubRequest RETURN LOGICAL
(INPUT  iiOrigRequest AS INT).

   DEF BUFFER bRequest  FOR MsRequest.

   FOR EACH bRequest WHERE
            bRequest.OrigRequest = iiOrigRequest NO-LOCK.
                     
      IF bRequest.Mandatory   = 1 AND
         LOOKUP(STRING(bRequest.ReqStatus),"2,4,9") = 0 THEN RETURN FALSE.

   END.
   
   RETURN TRUE.

END.

FUNCTION fGetSubRequestState RETURNS INTEGER
   (INPUT iiOrigRequest AS INT):

   DEF BUFFER bRequest  FOR MsRequest.

   DEF VAR liStatus AS INT  NO-UNDO.

   liStatus = 2.
   
   FOR EACH bRequest NO-LOCK WHERE
            bRequest.OrigRequest = iiOrigRequest AND
            bRequest.Mandatory   = 1:

      IF bRequest.ReqStatus = 3 OR 
         bRequest.ReqStatus = 4 
      THEN RETURN bRequest.ReqStatus.
      
      IF bRequest.ReqStatus NE 2 THEN liStatus = bRequest.ReqStatus.      
   END.

   RETURN liStatus.
END.

/* change status of request */
FUNCTION fReqStatus RETURNS LOGICAL
   (iiStatus AS INT,
    icMemo   AS CHAR).

   DEF VAR liMainStatus AS INT  NO-UNDO.
   DEF BUFFER bRequest  FOR MsRequest.

   MSREQUEST:
   DO TRANS:
      FIND bRequest WHERE ROWID(bRequest) = ROWID(MsRequest)
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

      /* another process is handling this */
      IF LOCKED(bRequest) THEN RETURN FALSE.
      
      ASSIGN bRequest.UpdateStamp = Func.Common:mMakeTS()
             bRequest.ReqStatus   = iiStatus.

      IF bRequest.ReqStatus >= 2 AND bRequest.ReqStatus <= 4 THEN 
         bRequest.DoneStamp = Func.Common:mMakeTS().

      IF icMemo > "" THEN 
      bRequest.Memo = bRequest.Memo + 
                      (IF bRequest.Memo > ""
                       THEN ", " 
                       ELSE "") + icMemo.
      
      IF MsRequest.OrigRequest > 0 AND MsRequest.Mandatory > 0 THEN DO:
      
          liMainStatus = ?.
          
          /* all subrequests have been succesfully handled */
          IF MsRequest.ReqStatus = 2 AND 
             fChkSubRequest(MsRequest.OrigRequest) 
          THEN liMainStatus = 8.
          
          /* mark main status to error if subrequest failed or was cancelled */
          ELSE IF MsRequest.ReqStatus = 3 OR MsRequest.ReqStatus = 4 
          THEN liMainStatus = 3. 
           
          IF liMainStatus NE ? THEN DO:   
             FIND bRequest WHERE 
                  bRequest.MSRequest  = MSRequest.OrigRequest
             EXCLUSIVE-LOCK NO-ERROR.
                   
             IF AVAILABLE bRequest THEN DO:
                /* If subrequest is handled properly after rejection then
                   mark main request status to 8 from 3 */
                IF (liMainStatus = 8 AND (bRequest.ReqStatus = 3 OR
                                          bRequest.ReqStatus = 7)) OR
                   (liMainStatus NE 8 AND bRequest.ReqStatus NE 3 AND 
                    bRequest.ReqStatus NE 4)
                THEN DO:
                   
                   ASSIGN bRequest.UpdateStamp = Func.Common:mMakeTS()
                          bRequest.ReqStatus   = liMainStatus.
                         
                   IF liMainStatus = 3 THEN       
                      bRequest.Memo = bRequest.Memo + 
                                      (IF bRequest.Memo > ""
                                       THEN ", " 
                                       ELSE "") +
                                      "Subrequest " + 
                                      STRING(MsRequest.ReqType) + " failed".
                 END.            
             END.   
          END.
      END.
      
      RELEASE bRequest.
   END. 
 
   RETURN TRUE.
    
END FUNCTION. 

/* change status of request */
FUNCTION fChangeReqStatus RETURNS LOGICAL
   (iiMsRequest AS INT,
    iiStatus    AS INT,
    icMemo      AS CHAR).

   DEF VAR liMainStatus AS INT  NO-UNDO.
   DEF BUFFER bRequest  FOR MsRequest.
   DEFINE VARIABLE liOrigRequest AS INTEGER NO-UNDO.
   DEFINE VARIABLE liReqType     AS INTEGER NO-UNDO.

   MSREQUEST:
   DO TRANS:
      FIND bRequest WHERE bRequest.MsRequest = iiMsRequest
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

      /* another process is handling this */
      IF NOT AVAILABLE bRequest OR LOCKED(bRequest) THEN RETURN FALSE.

      ASSIGN bRequest.UpdateStamp = Func.Common:mMakeTS()
             bRequest.ReqStatus   = iiStatus
             liOrigRequest        = bRequest.OrigRequest
             liReqType            = bRequest.ReqType
             .

      IF bRequest.ReqStatus >= 2 AND bRequest.ReqStatus <= 4 THEN
         bRequest.DoneStamp = Func.Common:mMakeTS().

      IF icMemo > "" THEN
      bRequest.Memo = bRequest.Memo +
                      (IF bRequest.Memo > ""
                       THEN ", "
                       ELSE "") + icMemo.

      IF bRequest.OrigRequest > 0 AND bRequest.Mandatory > 0 THEN DO:

          liMainStatus = ?.

          /* all subrequests have been succesfully handled */
          IF bRequest.ReqStatus = 2 AND
             fChkSubRequest(bRequest.OrigRequest)
          THEN liMainStatus = 8.

          /* mark main status to error if subrequest failed or was cancelled */
          ELSE IF bRequest.ReqStatus = 3 OR bRequest.ReqStatus = 4
          THEN liMainStatus = 3.

          IF liMainStatus NE ? THEN DO:
             FIND bRequest WHERE
                  bRequest.MSRequest  = liOrigRequest
             EXCLUSIVE-LOCK NO-ERROR.

             IF AVAILABLE bRequest THEN DO:
                /* If subrequest is handled properly after rejection then
                   mark main request status to 8 from 3 */
                IF (liMainStatus = 8 AND (bRequest.ReqStatus = 3 OR
                                          bRequest.ReqStatus = 7)) OR
                   (liMainStatus NE 8 AND bRequest.ReqStatus NE 3 AND
                    bRequest.ReqStatus NE 4)
                THEN DO:

                   ASSIGN bRequest.UpdateStamp = Func.Common:mMakeTS()
                          bRequest.ReqStatus   = liMainStatus.

                   IF liMainStatus = 3 THEN
                      bRequest.Memo = bRequest.Memo +
                                      (IF bRequest.Memo > ""
                                       THEN ", "
                                       ELSE "") +
                                      "Subrequest " +
                                      STRING(liReqType) + " failed".
                 END.
             END.
          END.
      END.

      RELEASE bRequest.
   END.

   RETURN TRUE.

END FUNCTION.

/* write to log file */
FUNCTION fReqLog RETURNS LOGICAL
   (icMessage AS CHAR).

   IF lcEventDir = "" THEN DO:
      lcEventDir = fCParamC(IF SESSION:BATCH
                            THEN "RequestLogDir"
                            ELSE "EventLogDir").
      IF lcEventDir = "" THEN lcEventDir = "/tmp".
   END.                                      
      
   OUTPUT STREAM sReqLog TO VALUE(lcEventDir + "/" + "RequestLog_" + 
                                  STRING(YEAR(TODAY),"9999") + 
                                  STRING(MONTH(TODAY),"99")  +
                                  STRING(DAY(TODAY),"99")    +
                                  ".log") APPEND.
                                  
   PUT STREAM sReqLog UNFORMATTED
      STRING(TIME,"hh:mm:ss") ";"
      MsRequest.MsRequest     ";"
      MsRequest.MsSeq         ";"
      MsRequest.CLI           ";"
      MsRequest.ReqType       ";"
      icMessage
      SKIP.
      
   OUTPUT STREAM sReqLog CLOSE.

END FUNCTION.

/* error occurred -> write to log and mark new status for request */
FUNCTION fReqError RETURNS LOGICAL
   (icError AS CHAR).

   fReqLog("ERROR " + icError).
   
   fReqStatus(3,icError).

END FUNCTION.

FUNCTION fMakeServiceSolog RETURNS INTEGER
   (iiMsRequest AS INT,
    output ocError AS CHAR):

   DEF VAR liSologCnt AS INT NO-UNDO.
   
   /* create solog from services */
   RUN Mm/setms.p(iiMSRequest,
             TRUE,
             OUTPUT liSologCnt,
             OUTPUT ocerror).
   
   /* -1: error occurred 
       0: there was no need to create solog
       1: solog created 
   */
   RETURN liSologCnt.
   
END FUNCTION.

/* checks if a status change of request is possible */
FUNCTION fChkReqStatusChange RETURNS LOG
   (iiStatus AS INT):
   
   DEF VAR i AS INT NO-UNDO.
   
   FIND MsReqStatFunc NO-LOCK WHERE
        MsReqStatFunc.ReqType   = MsRequest.ReqType AND
        MsReqStatFunc.ReqStatus = MsRequest.ReqStatus
   NO-ERROR.
   
   IF AVAIL MsReqStatFunc THEN DO:
      DO i = 2 TO NUM-ENTRIES(MsReqStatFunc.FuncGroup,","):
         FIND MsReqFuncItem NO-LOCK WHERE
              MsReqFuncItem.ItemId = ENTRY(i,MsReqStatFunc.FuncGroup,",")
         NO-ERROR.
         IF AVAIL MsReqFuncItem AND iiStatus = MsReqFuncItem.IParam THEN DO:
            RETURN TRUE.
         END.
      END.
   END.
   
   RETURN FALSE.
END.

FUNCTION fChangeOrderStatus RETURNS LOGICAL
   ( iiOrderID     AS INTEGER,
     icOrderStatus AS CHARACTER ):

   IF NOT iiOrderID > 0
   THEN RETURN FALSE.

   DEFINE BUFFER Order FOR Order.

   FOR Order NO-LOCK WHERE
       Order.Brand   = Syst.Var:gcBrand AND
       Order.OrderId = iiOrderId:

      IF Order.StatusCode NE icOrderStatus
      THEN DO TRANSACTION:
         BUFFER Order:FIND-CURRENT(EXCLUSIVE-LOCK).
         Order.StatusCode = icOrderStatus.
         RELEASE Order.
      END.

      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

&ENDIF
