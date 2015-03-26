/* acc_sendsms.p    12.05.09/aam

  send sms notification on acc events 

  callers e.g.: msagrcustchg.p, creditcheckquery.p, chmsreqst.p         
*/


{commali.i}
{msreqfunc.i}
{fhdrtext.i}

DEF INPUT PARAMETER iiMsRequest AS INT  NO-UNDO.
DEF INPUT PARAMETER iiCustNum   AS INT  NO-UNDO.
DEF INPUT PARAMETER icMessage   AS CHAR NO-UNDO.
DEF INPUT PARAMETER icText      AS CHAR NO-UNDO.

DEF VAR ldaFinalDay AS DATE NO-UNDO.
DEF VAR lcNewOwner  AS CHAR NO-UNDO.
DEF VAR liCreated   AS INT  NO-UNDO.
DEF VAR liHdrText   AS INT  NO-UNDO. 
DEF VAR ldeActStamp AS DEC NO-UNDO. 

FIND FIRST MsRequest WHERE MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 10 THEN 
   RETURN "ERROR:Unknown request".

FIND FIRST MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.

FIND FIRST Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN 
   RETURN "ERROR:Unknown customer".
   
lcSMSText = fGetSMSTxt("ACC_" + icMessage,
                    TODAY,
                    Customer.Language,
                    OUTPUT ldeActStamp). /* not used in this case */

IF NOT lcSMSText > "" THEN RETURN.

IF icText BEGINS "HT:" THEN DO:
   liHdrText = 0.
   liHdrText = INTEGER(ENTRY(2,icText,":")) NO-ERROR.
   IF liHdrText > 0 THEN 
      icText = fGetHdrText(liHdrText,Customer.Language).
   ELSE icText = "".   
END.

ASSIGN
   lcNewOwner = IF ENTRY(12,MsRequest.ReqCParam1,";") = "CIF"
                THEN ENTRY(5,MsRequest.ReqCParam1,";")
                ELSE ENTRY(2,MsRequest.ReqCParam1,";") + " " + 
                     ENTRY(1,MsRequest.ReqCParam1,";") + " " +
                     ENTRY(3,MsRequest.ReqCParam1,";")
   lcSMSText  = REPLACE(lcSMSText,"#NewOwner",lcNewOwner)
   lcSMSText  = REPLACE(lcSMSText,"#Description",icText).
           
/* tags */
IF AVAILABLE MobSub THEN 
   fReplaceSMS(lcSMSText,
               MsRequest.MsSeq,
               TODAY,
               OUTPUT lcSMSText).

/* send notification on previous night at 19 */
IF icMessage = "PreviousDay" AND MsRequest.ReqDParam1 NE 0 THEN DO:
   fSplitTS(MsRequest.ReqDParam1,
            OUTPUT ldaFinalDay,
            OUTPUT liReqCnt).
   ldReqStamp = fMake2DT(ldaFinalDay - 1,68400).         
END.
   
ELSE DO:
   ldReqStamp = DYNAMIC-FUNCTION("fMakeOfficeTS" in ghFunc1).
   IF ldReqStamp = ? THEN ldReqStamp = fMakeTS().
END.
   
liCreated = fMakeSchedSMS(iiCustNum,
                          MsRequest.CLI,
                          20,
                          lcSMSText,
                          ldReqStamp).
                          
IF liCreated > 0 AND icMessage = "PreviousDay" THEN 
FOR FIRST CallAlarm EXCLUSIVE-LOCK WHERE
          CallAlarm.Brand = gcBrand       AND
          CallAlarm.CLI   = MsRequest.CLI AND
          CallAlarm.DeliStat = 1 AND
          CallAlarm.CASeq    = liCreated:
   CallAlarm.DeliPara = "PD".        
END.          
