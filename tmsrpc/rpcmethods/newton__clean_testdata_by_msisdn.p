/**
 * Clean test data of given MSIDN.

 * @input string;mandatory;MSISDN
 * @output boolean;TRUE
 */

{xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Func/msisdn.i}

DEF VAR lcMSISDN        AS CHAR NO-UNDO.
DEF VAR MSISDN_status   AS INT  NO-UNDO FORMAT "Z9".

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

lcMSISDN = get_string(param_toplevel_id,"0").

IF lcMSISDN = "" OR lcMSISDN = ? THEN
   RETURN appl_err("MSISDN is empty").

FUNCTION fDeleteOrder RETURNS LOG (INPUT icMSISDN AS CHAR):

   FOR EACH Order WHERE
            Order.CLI = icMSISDN EXCLUSIVE-LOCK:
       FOR EACH OrderCustomer OF Order EXCLUSIVE-LOCK:
          DELETE OrderCustomer.
       END. /* FOR EACH OrderCustomer OF Order EXCLUSIVE-LOCK: */

      DELETE Order.
   END. /* FOR EACH Order WHERE */

   RETURN TRUE.
END FUNCTION.

FUNCTION fReleaseSIM RETURNS LOG (INPUT icICC AS CHAR):

   FOR FIRST SIM EXCLUSIVE-LOCK WHERE
             SIM.Brand EQ gcBrand AND
             SIM.ICC   EQ icICC   AND
            (SIM.Stock EQ "TESTING" OR
             SIM.Stock EQ "EMATESTING") AND
             SIM.SimStat <> 1:
      SIM.SimStat = 1.
   END.

   RETURN TRUE.
END FUNCTION.

FUNCTION fReleaseMSISDN RETURNS LOG (INPUT icMSISDN AS CHAR):

/* Check if MSISDN in EMA range. If belongs to EMA then set status to 98 */
   IF fIsEmaMsisdn(icMSISDN) THEN MSISDN_status = 98.
   ELSE MSISDN_status = 99.   /* use normal status value */

   FOR FIRST MSISDN EXCLUSIVE-LOCK WHERE
             MSISDN.Brand = gcBrand AND
             MSISDN.CLI   = icMSISDN AND
             MSISDN.StatusCode < 98:
      fMakeMsidnHistory(INPUT RECID(MSISDN)).
      ASSIGN MSISDN.OrderId = 0
             Msisdn.MsSeq   = 0
             MSISDN.StatusCode = MSISDN_status.
   END.

   RETURN TRUE.
END FUNCTION.

FUNCTION fDeleteSubscription RETURNS LOG (INPUT icMSISDN AS CHAR):

   FOR EACH MsOwner WHERE
            MsOwner.CLI = icMSISDN EXCLUSIVE-LOCK:

       FOR EACH FixedFee USE-INDEX Custnum WHERE
                FixedFee.Brand     = gcBrand AND
                FixedFee.Custnum   = MsOwner.CustNum AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue  = STRING(MsOwner.MsSeq) EXCLUSIVE-LOCK:
          FOR EACH FFItem OF FixedFee EXCLUSIVE-LOCK:
             DELETE FFItem.
          END.
          DELETE FixedFee.
       END. /* FOR EACH FixedFee USE-INDEX HostTable WHERE */

       FOR EACH SingleFee USE-INDEX Custnum WHERE
                SingleFee.Brand     = gcBrand AND
                SingleFee.Custnum   = MsOwner.CustNum AND
                SingleFee.HostTable = "Mobsub" AND
                SingleFee.KeyValue  = STRING(MsOwner.MsSeq) EXCLUSIVE-LOCK:
          DELETE SingleFee.
       END.

       FOR EACH DCCLI WHERE
                DCCLI.MsSeq = MsOwner.MsSeq EXCLUSIVE-LOCK:
          DELETE DCCLI.
       END.

       FOR EACH MServiceLimit WHERE
                MServiceLimit.MsSeq = MsOwner.MsSeq EXCLUSIVE-LOCK:
          DELETE MServiceLimit.
       END.

       FOR EACH MServiceLimit WHERE
                MServiceLimit.CustNum = MsOwner.CustNum EXCLUSIVE-LOCK:
          DELETE MServiceLimit.
       END.

       FOR EACH ServiceLcounter WHERE
                ServiceLcounter.MsSeq = MsOwner.MsSeq EXCLUSIVE-LOCK:
          DELETE ServiceLcounter.
       END.

       FOR EACH ServiceLcounter WHERE
                ServiceLcounter.CustNum = MsOwner.CustNum EXCLUSIVE-LOCK:
          DELETE ServiceLcounter.
       END.

       FOR EACH MServiceLPool WHERE
                MServiceLPool.MsSeq = MsOwner.MsSeq EXCLUSIVE-LOCK:
          DELETE MServiceLPool.
       END.

       FOR EACH MServiceLPool WHERE
                MServiceLPool.CustNum = MsOwner.CustNum EXCLUSIVE-LOCK:
          DELETE MServiceLPool.
       END.

       FOR EACH MsRequest WHERE
                MsRequest.MsSeq = MsOwner.MsSeq EXCLUSIVE-LOCK:
          DELETE MsRequest.
       END.

       DELETE MsOwner.
   END. /* FOR EACH MsOwner WHERE */

   FOR EACH MobCDR WHERE
            MobCDR.CLI = icMSISDN EXCLUSIVE-LOCK:
      DELETE MobCDR.
   END.

   FOR EACH MobSub WHERE
            MobSub.Brand = gcBrand AND
            MobSub.CLI   = icMSISDN EXCLUSIVE-LOCK:
      DELETE MobSub.
   END.

   FOR EACH TermMobSub WHERE
            TermMobSub.Brand = gcBrand AND
            TermMobSub.CLI   = icMSISDN EXCLUSIVE-LOCK:
      DELETE TermMobSub.
   END.

   RETURN TRUE.
END FUNCTION.

/* Main Block */

FIND FIRST MobSub WHERE
           MobSub.Brand = gcBrand AND
           MobSub.CLI   = lcMSISDN NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN
   RETURN appl_err("Subscription not found").
ELSE DO:
   FIND FIRST SIM WHERE
              SIM.Brand EQ gcBrand    AND
              SIM.ICC   EQ MobSub.ICC AND
             (SIM.Stock EQ "TESTING" OR
              SIM.Stock EQ "EMATESTING") NO-LOCK NO-ERROR.
   IF NOT AVAIL SIM THEN
      RETURN appl_err("MSISDN does not belong to testing tool").
END.

fReleaseSIM(INPUT MobSub.ICC).
fReleaseMSISDN(INPUT MobSub.CLI).
fDeleteOrder(INPUT MobSub.CLI).
fDeleteSubscription(INPUT MobSub.CLI).

add_boolean(response_toplevel_id,?,True).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
