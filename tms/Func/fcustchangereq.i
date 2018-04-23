/* fcustchangereq.i      30.04.09/aam separated from fmakemsreq.i

   change customer to subscription
*/
&IF "{&FCUSTCHANGEREQ_I}" NE "YES"
&THEN

&GLOBAL-DEFINE FCUSTCHANGEREQ_I YES

{Syst/commali.i}
{Func/fcreatereq.i}

/* changes to subscription's customer data */
FUNCTION fMSCustChangeRequest RETURNS INTEGER
   (INPUT  iiMsSeq      AS INT,
    INPUT  icChgType    AS CHAR,  /* user / invcust / agrcust */
    INPUT  iiNewCust    AS INT,   /* new customer */
    INPUT  iiOldCust    AS INT,   /* old customer */
    INPUT  icNewData    AS CHAR,  /* surname;firstname;surname2;CO;company;
                                     address;zipcode;postoffice;country;
                                     email;salesman;idtype;dni;birthday;
                                     language;title;region;bankacc;nationality
                                   */
    INPUT  idChgStamp   AS DEC,
    INPUT  ilCreateFees AS LOG,
    INPUT  ideFee       AS DEC,
    INPUT  ilSendSMS    AS LOG,
    INPUT  icCreator    AS CHAR,   /* who made the request */
    INPUT  icSource     AS CHAR,   /* source of the request */
    INPUT  iiOrigReq    AS INT,  /* father request */
    INPUT  icContractID AS CHAR, /*contract id, dms*/
    OUTPUT ocResult     AS CHAR).

   DEF VAR liReqType    AS INT  NO-UNDO.
   DEF VAR ldFirstAct   AS DEC  NO-UNDO.
   DEF VAR liReqCreated AS INT  NO-UNDO.
   DEF VAR liOrderID    AS INT NO-UNDO. 

   DEF BUFFER bReqMobSub   FOR MobSub.   

   IF iiNewCust = 0 AND icNewData = "" THEN DO:
      ocResult = "Invalid new customer data".
      RETURN 0.
   END. 

   IF icNewData BEGINS "orderid:" THEN ASSIGN
      liOrderID = INT(ENTRY(2,icNewData,":")) 
      icNewData = "" NO-ERROR.

   CASE icChgType:
   WHEN "user"    THEN liReqType = 3.
   WHEN "invcust" THEN liReqType = 4.
   WHEN "agrcust" THEN liReqType = 10.
   OTHERWISE DO:
      ocResult = "Invalid change type".
      RETURN 0.
   END.
       
   END CASE. 
   
   ocResult = fChkRequest(iiMsSeq,
                          liReqType,
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.                       

   /* set activation time if caller has not determined it */
   IF idChgStamp = ? THEN idChgStamp = Func.Common:mMakeTS().

   /* 1. phase of agrcust change is always run immediately */ 
   IF icChgType = "agrcust" THEN ldFirstAct = Func.Common:mMakeTS().
   ELSE ldFirstAct = idChgStamp.
   
   fCreateRequest(liReqType,
                  ldFirstAct,
                  icCreator,
                  ilCreateFees,
                  ilSendSMS).

   ASSIGN 
      bCreaReq.ReqIParam1 = iiNewCust
      liReqCreated        = bCreaReq.MsRequest
      bCreaReq.CustNum    = iiOldCust
      bCreaReq.ReqCParam1 = icNewData
      bCreaReq.ReqCParam6 = icContractID
      bCreaReq.ReqCParam4 = "111"
      bCreaReq.ReqDParam1 = idChgStamp
      bCreaReq.ReqDParam2 = ideFee
      bCreaReq.ReqSource  = icSource
      bCreaReq.ReqIParam4 = liOrderID
      bCreaReq.OrigRequest = iiOrigReq. 

   /* ACC for Fusion tariffs */
   FIND FIRST bReqMobSub WHERE 
              bReqMobSub.MsSeq = iiMsSeq
              NO-LOCK NO-ERROR.
   IF AVAILABLE bReqMobSub AND
      LOOKUP(bReqMobSub.CLIType,"CONTFF,CONTSF") > 0 AND
      liReqType = 10
   THEN bCreaReq.ReqStatus  = 19. /* waiting for confirmation */
   
   RELEASE bCreaReq.
   
   RETURN liReqCreated.
             
END FUNCTION.

&ENDIF
