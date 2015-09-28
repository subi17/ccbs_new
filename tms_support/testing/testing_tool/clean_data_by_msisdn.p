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

DEF VAR lcminMSISDN     AS CHAR NO-UNDO FORMAT "X(10)".
DEF VAR lcmaxMSISDN     AS CHAR NO-UNDO FORMAT "X(10)".
DEF VAR liMSISDN_status AS INT  NO-UNDO FORMAT "Z9".
DEF VAR liCounter       AS INT NO-UNDO INIT 0.

FORM
   SKIP
   "Enter one MSISDN or range start: " lcminMSISDN SKIP
   "Enter range stop or leave empty: " lcmaxMSISDN SKIP
   WITH OVERLAY CENTERED ROW 10 TITLE " Testing Tool Data Clean up " NO-LABELS
   FRAME fclean.

UPDATE lcminMSISDN lcmaxMSISDN with FRAME fclean.

IF lcminMSISDN = "" THEN DO:
   MESSAGE "Please Enter MSISDN" VIEW-AS ALERT-BOX.
   RETURN.
END.

IF lcmaxMSISDN = "" THEN lcmaxMSISDN = lcminMSISDN.

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
   IF fIsEmaMsisdn(icMSISDN) THEN liMSISDN_status = 98.
   ELSE liMSISDN_status = 99.   /* use normal status value */

   FOR FIRST MSISDN EXCLUSIVE-LOCK WHERE
             MSISDN.Brand = gcBrand AND
             MSISDN.CLI   = icMSISDN AND
             MSISDN.StatusCode < 98:
      fMakeMsidnHistory(INPUT RECID(MSISDN)).
      ASSIGN MSISDN.OrderId = 0
             Msisdn.MsSeq   = 0
             MSISDN.StatusCode = liMSISDN_status.
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

FOR EACH MSISDN NO-LOCK WHERE
         MSISDN.Brand = gcBrand AND
         MSISDN.CLI   GE lcminMSISDN AND
         MSISDN.CLI   LE lcmaxMSISDN AND
         MSISDN.ValidTo GE fMakeTS() AND
         MSISDN.StatusCode < 98:

   FIND FIRST Order WHERE
              Order.Brand = gcBrand AND
              Order.CLI   = MSISDN.CLI AND
              Order.OrderType = 0 NO-LOCK NO-ERROR.
   IF NOT AVAIL Order THEN DO:
      DISPLAY "MSISDN nro: " + MSISDN.CLI + " Order is not available, nothing to revert" FORMAT "X(70)".
      NEXT.
   END.
   ELSE DO:
      FIND FIRST SIM WHERE
                 SIM.Brand EQ gcBrand   AND
                 SIM.ICC   EQ Order.ICC AND
                (SIM.Stock EQ "TESTING" OR
                 SIM.Stock EQ "EMATESTING") NO-LOCK NO-ERROR.
      IF NOT AVAIL SIM THEN DO:
         DISPLAY "MSISDN nro: " + MSISDN.CLI + " SIM does not belong to testing tool" FORMAT "X(70)". 
         NEXT.
      END.
      ELSE DO:
         liCounter = liCounter + 1.
         fReleaseSIM(Order.ICC).
         fReleaseMSISDN(INPUT MSISDN.CLI).
         fDeleteOrder(INPUT MSISDN.CLI).
         fDeleteSubscription(INPUT MSISDN.CLI).
      END.
   END.
END.
DISPLAY liCounter " MSISDNs cleaned." FORMAT "X(70)" SKIP.
