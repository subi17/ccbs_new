DEF VAR oiValue AS INT NO-UNDO.

{billableduration.i}
{error_codes.i}

FUNCTION fBillableDuration RETURN INT
  (INPUT icCallCase   AS CHAR,
   INPUT icBDest      AS CHAR,
   INPUT idaEventDate AS DATE,
   INPUT iiBillDur    AS INT):

   DEF VAR liDuration  AS INT  NO-UNDO.
   DEF VAR liCycle     AS INT  NO-UNDO.
   
   IF icCallCase = "" THEN 
      FIND FIRST ttDuration WHERE 
                 ttDuration.CallCase = "CALL" 
      NO-LOCK NO-ERROR.

   ELSE DO:
      FIND FIRST ttDuration WHERE
                 ttDuration.CallCase  = icCallCase   AND
                 ttDuration.BDest     = icBDest      AND
                 ttDuration.ToDate   >= idaEventDate AND
                 ttDuration.FromDate <= idaEventDate NO-ERROR.
      IF NOT AVAILABLE ttDuration THEN            
      FIND FIRST ttDuration WHERE
                 ttDuration.CallCase  = icCallCase   AND
                 ttDuration.BDest     = ""           AND
                 ttDuration.ToDate   >= idaEventDate AND
                 ttDuration.FromDate <= idaEventDate NO-ERROR.
   END.              

   IF NOT AVAILABLE ttDuration THEN RETURN iiBillDur.
   
   IF icCallCase = "" THEN DO:
      IF  iiBilldur <= ttDuration.FLimitCycle 
      THEN ASSIGN liDuration = ttDuration.FLimitCycle.
      ELSE DO:
         ASSIGN
         liCycle    = TRUNC(iiBillDur / ttDuration.SLimitCycle,0) - 1 
         liDuration = ttDuration.FLimitCycle + 
                      (liCycle * ttDuration.SLimitCycle).
      END.
   END.

   ELSE DO:
  
      ASSIGN
         liCycle    = MAX(0,TRUNC((iiBillDur - ttDuration.firstlimit) 
                                   / ttDuration.SLimitCycle,0))
         liDuration = ttDuration.FLimitCycle +
                     (liCycle * ttDuration.SLimitCycle).
   END.

   RETURN liDuration.
   
END.

FUNCTION fCallCaseCheck RETURNS INTEGER
   (iiCallCase AS INT,
    idaEventDate AS DATE):
      
   FIND FIRST ttTCC WHERE
              ttTCC.TCC = iiCallCase AND
              ttTCC.ValidTo >= idaEventDate AND
              ttTCC.ValidFrom <= idaEventDate NO-ERROR.
   IF AVAILABLE ttTCC THEN RETURN ttTCC.ErrorCode.
   
   RETURN 0.  
    
END FUNCTION.    
    
FUNCTION fTicketCheck RETURN LOG
  (INPUT  icCheckTarget AS CHAR,
   INPUT  icValue       AS CHAR,
   OUTPUT oiValue   AS INT):
   
   oiValue = 0.

   CASE icCheckTarget:
      
      WHEN "EVENT" THEN DO:
         IF       LOOKUP(icValue,"CALL,SMS,MMS,GPRS,WAP") = 0 THEN 
            oiValue = {&CDR_ERROR_UNKNOWN_CALL_MODULE_TYPE}.
         
      END.
      WHEN "GSMBNR" THEN DO:
      END.
      WHEN "BTYPE" THEN DO:
          IF      icValue =  "12" then oiValue = {&CDR_ERROR_EMERGENCY_CALL}.
      END.
      
      WHEN "MSOWNER" THEN DO:
      
         FIND FIRST msowner WHERE
                    msowner.brand    EQ gcBrand        AND
                    msowner.CLI       =  icValue       AND
                    msowner.tsend    >= CallTimeStamp  AND
                    msowner.tsbegin  <= CallTimeStamp NO-LOCK NO-ERROR.
                                                                       
         IF NOT AVAIL MSowner THEN DO:
            oiValue = {&CDR_ERROR_MSISDN_NOT_ACTIVE}.
            
            FIND FIRST Msowner WHERE 
                       Msowner.Cli = icValue NO-LOCK NO-ERROR.
            IF Not Avail msowner THEN oiValue  = {&CDR_ERROR_UNKNOWN_MSISDN}.

         END.
            
      END.

      WHEN "MSOWNER_FIXED" THEN DO:

         FIND FIRST msowner WHERE
                    msowner.brand       EQ gcBrand        AND
                    msowner.FixedNumber EQ icValue        AND
                    msowner.tsend       GE CallTimeStamp  AND
                    msowner.tsbegin     LE CallTimeStamp NO-LOCK NO-ERROR.

         IF NOT AVAIL MSowner THEN DO:
            oiValue = {&CDR_ERROR_MSISDN_NOT_ACTIVE}.

            FIND FIRST Msowner WHERE
                       Msowner.brand EQ gcBrand AND
                       Msowner.fixednumber = icValue NO-LOCK NO-ERROR.

            IF Not Avail msowner THEN oiValue  = {&CDR_ERROR_UNKNOWN_MSISDN}.

         END.

      END.

      
      WHEN "IMSI" THEN DO:

         FIND FIRST msowner WHERE
                    msowner.IMSI       =  icValue      AND
                    msowner.tsend    >= CallTimeStamp  AND
                    msowner.tsbegin  <= CallTimeStamp  NO-LOCK NO-ERROR.

         IF NOT AVAIL MSowner THEN DO:
            oiValue = {&CDR_ERROR_MSISDN_NOT_ACTIVE}.
                                       
            FIND FIRST Msowner WHERE
                       Msowner.IMSI = icValue NO-LOCK NO-ERROR.

            IF Not Avail msowner THEN oiValue  = {&CDR_ERROR_UNKNOWN_MSISDN}.
            
         END.
         
         IF oiValue = 0 THEN ttCall.CLI = Msowner.CLI.
      END.

      WHEN "CASETYPE" THEN DO:
         IF      icValue = "V"   THEN oiValue = 4.
         ELSE IF icValue = "S"   THEN oiValue = 5.
         ELSE if icValue = "D"   THEN oiValue = 6.
         ELSE if icValue = "G"   THEN oiValue = 7.
         ELSE if icValue = "F"   THEN oiValue = 8.
         ELSE IF icValue = "M"   THEN oiValue = 9.
         ELSE IF icValue = "W"   THEN oiValue = 10.
         ELSE IF icValue = "H"   THEN oiValue = 11.
         ELSE if icValue = "C"   THEN oiValue = 12.
         ELSE if icValue = "A"   THEN oiValue = 17.
         ELSE                         oiValue = 0  .
      END.
   END CASE.
END.   


FUNCTION fRawTicketCheck RETURN INTEGER.

   IF   lcStartTime = "" OR 
        lcStartDate = ""   THEN RETURN  {&CDR_ERROR_INVALID_DATE_TIME}.
   ELSE IF ttCall.CLI      = "" AND 
           ttCall.Imsi2    = "" AND
           ttCall.Roaming  = 0 THEN RETURN  {&CDR_ERROR_INVALID_GSM_A_SUB}.
   ELSE IF TTCall.GSMBnr   = ""            AND 
           ttCall.RoutingNumber = ""       AND
           ttCall.EventType    NE "GPRS"   AND 
           ttCall.Roaming       = 0 THEN 
      RETURN  {&CDR_ERROR_GSM_B_NUMBER_MISSING_ON_MST}.
   ELSE RETURN 0.
END.

FUNCTION fFinalBillableCheck RETURNS INTEGER:
 
   /* free call forwarding to voice mail is not billed */
   IF ttCall.SpoCmt = 30 AND
      LOOKUP(ttCall.GsmBnr,"633,633633633") > 0 AND
      ttCall.CCN = 98 AND
      ttCall.Amount = 0 
   THEN RETURN {&CDR_ERROR_FREE_VOICE_MAIL_CALL_FORWARDING}.
   
   RETURN 0.
   
END FUNCTION.


