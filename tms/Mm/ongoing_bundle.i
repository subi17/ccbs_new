&IF "{&ONGOING_BUNDLE}" NE "YES" 
&THEN
&GLOBAL-DEFINE ONGOING_BUNDLE YES

{Syst/tmsconst.i}
{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}

FUNCTION fGetActOngoingDataBundles RETURNS CHAR
   (iiMsSeq     AS INT,
    idActStamp  AS DEC):

   DEF VAR lcActiveBundles   AS CHAR NO-UNDO.

   DEF VAR lcPostpaidDataBundles  AS CHAR NO-UNDO.
   DEF VAR lcPrePaidDataBundles   AS CHAR NO-UNDO.

   DEF BUFFER bMsRequest     FOR MsRequest.

   IF idActStamp = 0 OR idActStamp = ? THEN
      idActStamp = fMakeTS().

   ASSIGN lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
          lcPrePaidDataBundles  = fCParamC("PREPAID_DATA_CONTRACTS").

   /* Check active bundle */
   FOR EACH MServiceLimit NO-LOCK WHERE
            MServiceLimit.MSSeq = iiMsSeq AND
            MServiceLimit.EndTS >= idActStamp,
      FIRST ServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
            ServiceLimit.SlSeq = MServiceLimit.SlSeq AND
            LOOKUP(ServiceLimit.GroupCode,
                   lcPostpaidDataBundles + "," + lcPrePaidDataBundles) > 0 AND
            ServiceLimit.GroupCode <> "TARJD1":
      lcActiveBundles = lcActiveBundles +
                        (IF lcActiveBundles > "" THEN "," ELSE "") +
                         ServiceLimit.GroupCode.
   END. /* FOR EACH MServiceLimit NO-LOCK WHERE */

   lcActiveBundles = TRIM(lcActiveBundles,",").

   IF lcActiveBundles > "" THEN RETURN lcActiveBundles.

   /* Check any ongoing active bundle request */
   FOR EACH bMsRequest NO-LOCK WHERE
            bMsRequest.MsSeq = iiMsSeq AND
            bMsRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
            LOOKUP(STRING(bMsRequest.ReqStatus),
                   {&REQ_INACTIVE_STATUSES} + ",3") = 0 AND
            LOOKUP(bMsRequest.ReqCParam3,
                   lcPostpaidDataBundles + "," + lcPrePaidDataBundles) > 0 AND
                   bMsRequest.ReqCParam3 <> "TARJD1":
      lcActiveBundles = lcActiveBundles +
                        (IF lcActiveBundles > "" THEN "," ELSE "") +
                         bMsRequest.ReqCParam3.
   END. /* FOR EACH bMsRequest WHERE */

   lcActiveBundles = TRIM(lcActiveBundles,",").

   RETURN lcActiveBundles.

END FUNCTION.

&ENDIF
