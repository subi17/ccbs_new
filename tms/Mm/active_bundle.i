&IF "{&ACTIVE_BUNDLE}" NE "YES" 
&THEN
&GLOBAL-DEFINE ACTIVE_BUNDLE YES

{Syst/tmsconst.i}
{Mm/bundle_type.i}

FUNCTION fGetActiveSpecificBundle RETURNS CHAR
   (iiMsSeq      AS INT,
    idActiveTime AS DEC,
    icBundleType AS CHAR):

   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER bServiceLimit  FOR ServiceLimit.

   DEF VAR lcContracts       AS CHAR NO-UNDO.

   lcContracts = fGetBundles(icBundleType).
   IF lcContracts = "" THEN RETURN "".

   FIND_BUNDLES:
   FOR EACH bMServiceLimit NO-LOCK WHERE
            bMServiceLimit.MsSeq   = iiMsSeq AND
            bMServiceLimit.EndTS  >= idActiveTime, 
      FIRST bServiceLimit NO-LOCK USE-INDEX SLSeq WHERE
            bServiceLimit.SLSeq = bMServiceLimit.SLSeq AND
     LOOKUP(bServiceLimit.GroupCode,lcContracts) > 0:

      /* pending termination request */
      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.MsSeq = iiMsSeq AND
                        MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                        LOOKUP(STRING(MsRequest.ReqStatus),
                                {&REQ_INACTIVE_STATUSES}) = 0 AND
                        MsRequest.ActStamp <= idActiveTime) THEN
         NEXT FIND_BUNDLES.       

      RETURN bServiceLimit.GroupCode.
   END.

   RETURN "".
    
END FUNCTION.

FUNCTION fGetCurrentSpecificBundle RETURNS CHAR
   (iiMsSeq AS INT,
    icContractType AS CHAR):

   RETURN fGetActiveSpecificBundle(iiMsSeq,
                                   fMakeTS(),
                                   icContractType).
END FUNCTION.

&ENDIF
