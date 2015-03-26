/*-----------------------------------------------------------------------------
  MODULE .......: saldopaym.p
  FUNCTION .....: additional payment to saldo agreement limit
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 27.12.05
  CHANGED.. ....: 13.03.06/aam separated from msrequest.i
  Version ......: M15
  -------------------------------------------------------------------------- */


{msreqfunc.i}
{fsubser.i}
{fcpfat.i}
{msopenbatch.i}

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.


FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN RETURN "ERROR".

lcReqType = "spreq".

RUN pSaldoPayment.

RETURN RETURN-VALUE.


PROCEDURE pSaldoPayment:

   DEF VAR llReqFound    AS LOG  NO-UNDO. 
   DEF VAR lcPaymType    AS CHAR NO-UNDO.
   DEF VAR lcFatGroup    AS CHAR NO-UNDO.
   DEF VAR lcOtherGroup  AS CHAR NO-UNDO. 
   DEF VAR liFatPeriod   AS INT  NO-UNDO.
   DEF VAR ldUnbilled    AS DEC  NO-UNDO.
   DEF VAR liCreditType  AS INT  NO-UNDO.
   DEF VAR liMobSubLimit AS INT  NO-UNDO. 
   DEF VAR ldMobSubLimit AS DEC  NO-UNDO.
   DEF VAR ldLimitPerc   AS DEC  NO-UNDO.
   DEF VAR liDefLimCust  AS INT  NO-UNDO.
    
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE MobSub THEN DO:
      fReqError("MobSub not found").
      RETURN.
   END.

   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).
        
   /* payment amount must be a positive sum */
   IF MsRequest.ReqDParam1 <= 0 THEN DO:
      fReqError("Nothing to do").
      RETURN. 
   END.

   /* what fat group should be used */
   IF MsRequest.ReqCParam1 = "saldo" THEN ASSIGN 
      lcFatGroup   = "SaldoAgreementAccount"
      lcOtherGroup = "eGiftPayment".
   ELSE IF MsRequest.ReqCParam1 = "egift" THEN ASSIGN 
      lcFatGroup   = "eGiftPayment"
      lcOtherGroup = "SaldoAgreementAccount". 
   ELSE DO:
      fReqError("Payment type unknown").
      RETURN. 
   END. 
   
   lcFatGroup = fCParamC(lcFatGroup).
   
   IF lcFatGroup = ? OR lcFatGroup = "" THEN DO:
      fReqError("FAT for " + lcPaymType + " not defined").
      RETURN. 
   END.
   
   /* fat is always created for current period */
   liFatPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).
   
   /* create a new fatime row */
   IF NOT fCreateFatime(lcFatGroup,
                        MobSub.CustNum,
                        MobSub.MsSeq,
                        MobSub.CLI,
                        MsRequest.ReqDParam1,
                        liFatPeriod,
                        "")
   THEN DO:
      fReqError("FAT could not be created").

      /* send a sms ? */
      
      RETURN. 
   END. 

   /* if mobsub is closed (due to saldo limit) then check balance */
   IF LOOKUP(STRING(MobSub.MsStat),"7,37") > 0 THEN DO:
   
      /* get current unbilled saldo */
      ldUnbilled = 0.
      FOR EACH SaldoCounter NO-LOCK WHERE
               SaldoCounter.MsSeq  = MobSub.MsSeq AND
               SaldoCounter.Period = liFatPeriod:
         ldUnbilled = ldUnbilled + SaldoCounter.amt.
      END.

     
      /* mobsub's limit */
      liCreditType = fCreditTypeValue(MobSub.MsSeq,
                                      OUTPUT liMobSubLimit).
      
      /* saldo agreement */
      IF liCreditType = 3 THEN DO:
      
         /* payments that increase the limit */
         ldMobSubLimit = liMobSubLimit + 
                         DYNAMIC-FUNCTION("fChkSaldoAccount" IN ghFunc1,
                                          MobSub.CustNum,
                                          MobSub.CLI,
                                          liFatPeriod,
                                          lcFatGroup).
       
         /* also the other type of saldo fat must be checked */
         lcOtherGroup = fCParamC(lcOtherGroup).
         IF lcOtherGroup > "" THEN 
         ldMobSubLimit = ldMobSubLimit + 
                         DYNAMIC-FUNCTION("fChkSaldoAccount" IN ghFunc1,
                                          MobSub.CustNum,
                                          MobSub.CLI,
                                          liFatPeriod,
                                          lcOtherGroup).
          
         /* paid enough -> open subscription */
         IF ldMobSubLimit > ldUnbilled THEN DO:
 
            IF NOT fOpenSaldoBarring() THEN DO:
               /* could not be opened, what happens ? */  
            END. 
      
            ELSE DO:
               
               /* usage portion after limit raise */
               ldLimitPerc = ldUnbilled * 100 / ldMobSubLimit. 
               
               /* set counter limit backwards; find the alarm limit that
                  has been exceeded with the new usage portion and mark 
                  that to counter -> avoid redundant sms alarms */
               IF CAN-FIND(FIRST CallLimit WHERE 
                                 CallLimit.CLI        = MobSub.CLI AND 
                                 CallLimit.CreditType = liCreditType) 
               THEN DO:
                  FOR EACH CallLimit NO-LOCK WHERE
                           CallLimit.CLI        = MobSub.CLI  AND
                           CallLimit.dto       >= TODAY       AND
                           CallLimit.dfrom     <= TODAY       AND
                           CallLimit.Limit     <= ldLimitPerc AND
                           CallLimit.CreditType = liCreditType
                  BY CallLimit.Limit DESC:
                     ldLimitPerc = CallLimit.Limit.
                     LEAVE.
                  END.
               END.   
               ELSE DO:

                  IF liDefLimCust = 0 THEN
                     liDefLimCust = fCParamI("DefCreditLimitMCust").
                     
                  IF liDefLimCust > 0 THEN 
                  FOR EACH CallLimit NO-LOCK WHERE
                           CallLimit.CustNo     = liDefLimCust AND
                           CallLimit.dto       >= TODAY        AND
                           CallLimit.dfrom     <= TODAY        AND
                           CallLimit.limit     <= ldLimitPerc  AND
                           CallLimit.CreditType = liCreditType
                  BY CallLimit.Limit DESC:
                     ldLimitPerc = CallLimit.Limit.
                     LEAVE.
                  END.
               END.      
            
               FOR EACH SaldoCounter EXCLUSIVE-LOCK WHERE
                        SaldoCounter.MsSeq  = MobSub.MsSeq AND
                        SaldoCounter.Period = liFatPeriod:
                  SaldoCounter.MobLimit = ldLimitPerc.
               END. 
            END.
            
         END. 
      END.
   
   END.                                 
      
   /* send SMS */
   IF MsRequest.SendSMS = 1 THEN DO:

      FIND Customer OF MobSub NO-LOCK.
      
      lcSMSText = fGetTxt("SMS",
                          "SALDOPAYM",
                          TODAY,
                          Customer.Language).

      IF lcSMSText > "" THEN DO:                    
         lcSMSText = REPLACE(lcSMSText,"#AMOUNT",
                                       TRIM(STRING(MsRequest.ReqDParam1,
                                                   ">>>>>>9.99"))).
             
         /* replace tags */
         fReplaceSMS(lcSMSText,
                     MsRequest.MsSeq,
                     TODAY,
                     OUTPUT lcSMSText).


         /* don't send messages before 8 am. */
         ldReqStamp = DYNAMIC-FUNCTION("fMakeOfficeTS" in ghFunc1).
         IF ldReqStamp = ? THEN ldReqStamp = fMakeTS().

         fMakeSchedSMS(MobSub.CustNum,
                       MobSub.CLI,
                       14,
                       lcSMSText,
                       ldReqStamp).
      END. 
   END.
 
   RELEASE MobSub.
                        
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.

 

