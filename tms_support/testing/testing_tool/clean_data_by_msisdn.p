/* ----------------------------------------------------------------------
  Module .......: clean_data_by_msisdn.p
  Task .........: Data clean up by MSISDn
  Application ..: TMS
  Author .......: Vikas
  Created ......: 11.07.12
  Version ......: Yoigo
---------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".
katun = "Qvantel".
{msisdn.i}

DEF VAR lcMSISDN   AS CHAR NO-UNDO FORMAT "X(10)".

FORM
   SKIP
   "Enter MSISDN:" lcMSISDN SKIP
   WITH OVERLAY CENTERED ROW 10 TITLE " Data Clean up " NO-LABELS
   FRAME fclean.

UPDATE lcMSISDN with FRAME fclean.

IF lcMSISDN = "" THEN DO:
   MESSAGE "Please Enter MSISDN" VIEW-AS ALERT-BOX.
   RETURN.
END.

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
             SIM.Stock EQ "TESTING" AND
             SIM.SimStat <> 1:
      SIM.SimStat = 1.
   END.

   RETURN TRUE.
END FUNCTION.

FUNCTION fReleaseMSISDN RETURNS LOG (INPUT icMSISDN AS CHAR):

   FOR FIRST MSISDN EXCLUSIVE-LOCK WHERE
             MSISDN.Brand = gcBrand AND
             MSISDN.CLI   = icMSISDN AND
             MSISDN.StatusCode <> 99:
      fMakeMsidnHistory(INPUT RECID(MSISDN)).
      ASSIGN MSISDN.OrderId = 0
             Msisdn.MsSeq   = 0
             MSISDN.StatusCode = 99.
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

FIND FIRST Order WHERE
           Order.Brand = gcBrand AND
           Order.CLI   = lcMSISDN AND
           Order.OrderType = 0 NO-LOCK NO-ERROR.
IF NOT AVAIL Order THEN DO:
   MESSAGE "Order is not available, nothing to revert" VIEW-AS ALERT-BOX.
   RETURN.
END.
ELSE DO:
   FIND FIRST SIM WHERE
              SIM.Brand EQ gcBrand   AND
              SIM.ICC   EQ Order.ICC AND
              SIM.Stock EQ "TESTING" NO-LOCK NO-ERROR.
   IF NOT AVAIL SIM THEN DO:
      MESSAGE "MSISDN does not belong to testing tool" VIEW-AS ALERT-BOX.
      RETURN.
   END.
END.

fReleaseSIM(Order.ICC).
fReleaseMSISDN(lcMSISDN).
fDeleteOrder(INPUT lcMSISDN).
fDeleteSubscription(INPUT lcMSISDN).


