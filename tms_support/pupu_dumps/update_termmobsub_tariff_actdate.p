
DISABLE TRIGGERS FOR LOAD OF MobSub.

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Func/timestamp.i}
{Mm/fbundle.i}

DEF VAR lcTariff     AS CHAR NO-UNDO.
DEF VAR liNum        AS INT  NO-UNDO.
DEF VAR ldaActDate   AS DATE NO-UNDO.

DEF BUFFER bTermMobSub   FOR TermMobSub.

OUTPUT TO "/store/riftp/pupu_dumps/logs/update_termmobsub_tariff_actdate.txt".

FUNCTION fGetTermTariff RETURNS CHAR
   (BUFFER TermMobsub FOR TermMobSub,
    OUTPUT odaActDate AS DATE):

   DEF BUFFER MServiceLimit FOR MServiceLimit.
   DEF BUFFER ServiceLimit  FOR ServiceLimit.
   DEF BUFFER MsOwner       FOR MsOwner.
   DEF BUFFER MsRequest     FOR MsRequest.

   DEF VAR lcTariffContract AS CHAR NO-UNDO.
   DEF VAR liTime           AS INT  NO-UNDO.
   DEF VAR ldeActStamp      AS DEC  NO-UNDO.
   DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.

   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").
   
   IF LOOKUP(TermMobsub.CLIType,lcBundleCLITypes) > 0 THEN
      lcTariffContract = TermMobsub.TariffBundle.

   IF lcTariffContract EQ "" THEN DO:
      FOR EACH MsOwner NO-LOCK WHERE
               MsOwner.MsSeq = TermMobsub.MsSeq USE-INDEX MsSeq:

         IF MsOwner.CLIType NE TermMobsub.CLIType THEN LEAVE.
         ldeActStamp = MsOwner.TSBegin.
      END.
      IF ldeActStamp > 0 THEN
         fSplitTS(ldeActStamp, OUTPUT odaActDate, OUTPUT liTime).

      RETURN TermMobsub.CLIType.
   END. /* IF lcTariffContract = "" THEN DO: */

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq = TermMobsub.MsSeq AND
            MsRequest.Reqtype = {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
            MsRequest.ReqCparam3 = lcTariffContract BY DoneStamp DESC:
      
      IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION}
      THEN NEXT.

      fSplitTS(MsRequest.ActStamp, OUTPUT odaActDate, OUTPUT liTime).
      RETURN MsRequest.ReqCparam3.
   END.
   
   RETURN "".
END.

FOR EACH MsRequest WHERE
         MsRequest.Brand = "1" AND
         MsRequest.ReqType = 18 AND
         MsRequest.ReqStatus = 2 AND
         MsRequest.ActStamp >= 20130901 NO-LOCK,
   FIRST TermMobSub WHERE
         TermMobSub.MsSeq = MsRequest.MsSeq NO-LOCK:

   liNum = liNum + 1.
   STATUS DEFAULT STRING(liNum).

   lcTariff = fGetTermTariff(BUFFER TermMobSub,OUTPUT ldaActDate).

   DO TRANSACTION:
      FIND FIRST bTermMobSub WHERE
                 ROWID(bTermMobSub) = ROWID(TermMobSub)
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bTermMobSub THEN DO:
         bTermMobSub.TariffActDate = ldaActDate.

         PUT UNFORMATTED
             STRING(TermMobSub.MsSeq) "|" TermMobSub.CLI "|" lcTariff "|"
             STRING(TermMobSub.CreationDate) "|" STRING(ldaActDate) "|" STRING(bTermMobSub.TariffActDate) SKIP.
      END.
      RELEASE TermMobSub.
  END.
END.

OUTPUT CLOSE.
