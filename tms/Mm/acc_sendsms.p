/* acc_sendsms.p    12.05.09/aam

  send sms notification on acc events 

  callers e.g.: msagrcustchg.p, creditcheckquery.p, chmsreqst.p         
*/


{Syst/commali.i}
{Func/msreqfunc.i}
{Func/fmakesms.i}

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
      icText = Func.Common:mGetHdrText(liHdrText,Customer.Language).
   ELSE icText = "".   
END.

IF MsRequest.ReqIParam4 > 0 THEN DO:
   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand = Syst.Var:gcBrand AND
              OrderCustomer.OrderID = MsRequest.ReqIParam4 AND
              OrderCustomer.RowType = {&ORDER_TYPE_ACC} NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN RETURN "".
   
   lcNewOwner = IF OrderCustomer.CustIDType EQ "CIF" 
                THEN OrderCustomer.Company
                ELSE OrderCustomer.FirstName + " " +
                     OrderCustomer.SurName1 + " " + 
                     OrderCustomer.SurName2.
END.
ELSE
   lcNewOwner = IF ENTRY(12,MsRequest.ReqCParam1,";") = "CIF"
                THEN ENTRY(5,MsRequest.ReqCParam1,";")
                ELSE ENTRY(2,MsRequest.ReqCParam1,";") + " " + 
                     ENTRY(1,MsRequest.ReqCParam1,";") + " " +
                     ENTRY(3,MsRequest.ReqCParam1,";").

ASSIGN
   lcSMSText  = REPLACE(lcSMSText,"#NewOwner",TRIM(lcNewOwner))
   lcSMSText  = REPLACE(lcSMSText,"#Description",icText).
           
/* tags */
IF AVAILABLE MobSub THEN
Func.Common:mReplaceSMS 
             ( Customer.CustName,
               Mobsub.CLI,
               lcSMSText,
               MsRequest.MsSeq,
               TODAY,
               OUTPUT lcSMSText).

/* send notification on previous night at 19 */
IF icMessage = "PreviousDay" AND MsRequest.ReqDParam1 NE 0 THEN DO:
   Func.Common:mSplitTS(MsRequest.ReqDParam1,
            OUTPUT ldaFinalDay,
            OUTPUT liReqCnt).
   ldReqStamp = Func.Common:mMake2DT(ldaFinalDay - 1,68400).         
END.
   
ELSE DO:
   ldReqStamp = Func.Common:mMakeOfficeTS().
   IF ldReqStamp = ? THEN ldReqStamp = Func.Common:mMakeTS().
END.
   
liCreated = fMakeSchedSMS(iiCustNum,
                          MsRequest.CLI,
                          20,
                          lcSMSText,
                          ldReqStamp).
                          
IF liCreated > 0 AND icMessage = "PreviousDay" THEN 
FOR FIRST CallAlarm EXCLUSIVE-LOCK WHERE
          CallAlarm.Brand = Syst.Var:gcBrand       AND
          CallAlarm.CLI   = MsRequest.CLI AND
          CallAlarm.DeliStat = 1 AND
          CallAlarm.CASeq    = liCreated:
   CallAlarm.DeliPara = "PD".        
END.          
