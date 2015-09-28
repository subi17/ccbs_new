/* ----------------------------------------------------------------------
  Module .......: clean_data_by_customer.p
  Task .........: Data clean up by customer
  Application ..: TMS
  Author .......: Vikas
  Created ......: 11.07.12
  Version ......: Yoigo
---------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".
katun = "Qvantel".
{msisdn.i}

DEF VAR lcDNI           AS CHAR NO-UNDO FORMAT "X(15)".
DEF VAR MSISDN_status   AS INT  NO-UNDO FORMAT "Z9".

FORM
   SKIP
   "Enter Customer DNI:" lcDNI SKIP
   WITH OVERLAY CENTERED ROW 10 TITLE " Data Clean up " NO-LABELS
   FRAME fclean.

UPDATE lcDNI with FRAME fclean.

IF lcDNI = "" THEN DO:
   MESSAGE "Please Enter Customer DNI" VIEW-AS ALERT-BOX.
   RETURN.
END.

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

FUNCTION fDeleteOrder RETURNS LOG (INPUT iiCustNum AS INT):

   FOR EACH Order WHERE
            Order.CustNum = iiCustNum EXCLUSIVE-LOCK:
       FOR EACH OrderCustomer OF Order EXCLUSIVE-LOCK:
          DELETE OrderCustomer.
       END. /* FOR EACH OrderCustomer OF Order EXCLUSIVE-LOCK: */

       fReleaseSIM(Order.ICC).
       fReleaseMSISDN(Order.CLI).

      DELETE Order.
   END. /* FOR EACH Order WHERE */

   RETURN TRUE.
END FUNCTION.

FUNCTION fDeleteSubscription RETURNS LOG (INPUT iiCustNum AS INT):

   FOR EACH MsOwner WHERE
            MsOwner.Brand   = gcBrand AND
            MsOwner.CustNum = iiCustNum EXCLUSIVE-LOCK:

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

       FOR EACH MobCDR WHERE
                MobCDR.CLI = MsOwner.CLI EXCLUSIVE-LOCK:
           DELETE MobCDR.
       END.

       FOR EACH MobSub WHERE
                MobSub.Brand = gcBrand AND
                MobSub.CLI   = MsOwner.CLI EXCLUSIVE-LOCK:
          DELETE MobSub.
       END.

       FOR EACH TermMobSub WHERE
                TermMobSub.Brand = gcBrand AND
                TermMobSub.CLI   = MsOwner.CLI EXCLUSIVE-LOCK:
          DELETE TermMobSub.
       END.

       DELETE MsOwner.
   END. /* FOR EACH MsOwner WHERE */

   RETURN TRUE.
END FUNCTION.

/* Main Block */

FOR EACH Customer WHERE
         Customer.Brand = gcBrand AND
         Customer.OrgId = lcDNI NO-LOCK,
    EACH MobSub WHERE
         MobSub.Brand   = gcBrand AND
         MobSub.AgrCust = Customer.CustNum NO-LOCK:

   FIND FIRST SIM WHERE
              SIM.Brand EQ gcBrand    AND
              SIM.ICC   EQ MobSub.ICC AND
             (SIM.Stock EQ "TESTING" OR
              SIM.Stock EQ "EMATESTING") NO-LOCK NO-ERROR.
   IF NOT AVAIL SIM THEN DO:
      MESSAGE "One of the MSISDN does not belong to testing tool" VIEW-AS ALERT-BOX.
      RETURN.
   END.
END.

FOR EACH Customer WHERE
         Customer.Brand = gcBrand AND
         Customer.OrgId = lcDNI EXCLUSIVE-LOCK:

   fDeleteOrder(INPUT Customer.CustNum).
   fDeleteSubscription(INPUT Customer.CustNum).

   DELETE Customer.
END.


