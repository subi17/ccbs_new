&IF "{&SHARPERCONFID_I}" NE "YES" 
&THEN
&GLOBAL-DEFINE SHARPERCONFID_I YES

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/cparam2.i}

FUNCTION fGetShaperConfId RETURN CHAR
   (INPUT iiMsSeq AS INT,
    INPUT icDCevent AS CHARACTER,
    INPUT icShaperBase AS CHAR,
    INPUT idtDate AS DATE,
    INPUT icCliType AS CHAR):

   DEFINE VARIABLE lcDefParam    AS CHARACTER NO-UNDO format "x(40)".
   DEFINE VARIABLE lcTagValue    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOrgTagValue AS CHARACTER NO-UNDO.

   DEF BUFFER bDCPackage FOR DCServicePackage.
   DEF BUFFER bDCComponent FOR DCServiceComponent.
   DEF BUFFER bContSub FOR MobSub.
   DEF BUFFER bBundleAct FOR MServiceLimit.
   DEF BUFFER bBundleLimit FOR ServiceLimit.
   DEF BUFFER bContract FOR DayCampaign.

   DEFINE VARIABLE ldeActTime AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ldeEndTime AS DECIMAL NO-UNDO.

   IF idtDate = TODAY
   THEN ASSIGN
      ldeActTime = fMakeTS()
      ldeEndTime = ldeActTime.
   ELSE ASSIGN
      ldeActTime = fMake2Dt(idtDate,0)
      ldeEndTime = fMake2Dt(idtDate,86399).

   DEFINE VARIABLE lcBONOContracts AS CHARACTER NO-UNDO.

   FIND bContSub WHERE bContSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   
   IF icShaperBase EQ "" THEN
      FOR FIRST bDCPackage NO-LOCK WHERE
                bDCPackage.Brand     = gcBrand AND
                bDCPackage.DCEvent   = icDCevent AND
                bDCPackage.ServPac   = "shaper" AND
                bDCPackage.ToDate   >= idtDate AND
                bDCPackage.FromDate <= idtDate,
          FIRST bDCComponent NO-LOCK WHERE
                bDCComponent.DCServicePackageID =
                bDCPackage.DCServicePackageID AND
                bDCComponent.ServCom = "shaper" AND
                bDCComponent.ToDate   >= idtDate AND
                bDCComponent.FromDate <= idtDate:
         lcDefParam = bDCComponent.DefParam.
      END.
   ELSE lcDefParam = icShaperBase.

   IF INDEX(lcDefParam,"#ADDBUNDLE") > 0 THEN DO:
      ASSIGN lcTagValue = ""
             lcBONOContracts = fCParamC("BONO_CONTRACTS").

      FOR EACH bBundleAct NO-LOCK WHERE
               bBundleAct.MsSeq = iiMsSeq AND
               bBundleAct.DialType = {&DIAL_TYPE_GPRS} AND
               bBundleAct.EndTS > ldeEndTime,
         FIRST bBundleLimit NO-LOCK WHERE
               bBundleLimit.SlSeq = bBundleAct.SlSeq AND
               bBundleLimit.GroupCode NE icDCEvent   AND
        LOOKUP(bBundleLimit.GroupCode,lcBONOContracts) > 0,
         FIRST bContract NO-LOCK WHERE
               bContract.Brand = gcBrand AND
               bContract.DCEvent = bBundleLimit.GroupCode AND
               LOOKUP(bContract.DCType,
                      {&PERCONTRACT_RATING_PACKAGE}) > 0,
         FIRST bDCPackage NO-LOCK WHERE
               bDCPackage.Brand     = gcBrand AND
               bDCPackage.DCEvent   = bContract.DCEvent AND
               bDCPackage.ServPac   = "shaper" AND
               bDCPackage.ToDate   >= idtDate AND
               bDCPackage.FromDate <= idtDate,
         FIRST bDCComponent NO-LOCK WHERE
               bDCComponent.DCServicePackageID =
                  bDCPackage.DCServicePackageID AND
               bDCComponent.ServCom = "shaper" AND
               bDCComponent.ToDate   >= idtDate AND
               bDCComponent.FromDate <= idtDate:

         IF CAN-FIND(FIRST
               MsRequest NO-LOCK WHERE
               MsRequest.MsSeq = bBundleAct.MsSeq AND
               MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
               MsRequest.ReqCParam3 = bContract.DCEvent AND
               LOOKUP(STRING(MsRequest.ReqStatus),
                      {&REQ_INACTIVE_STATUSES}) = 0 AND
               MsRequest.ActStamp < ldeActTime) THEN NEXT.

         lcTagValue = bDCComponent.DefParam.
         LEAVE.
      END.
      IF lcTagValue > "" THEN lcDefParam = lcTagValue.
      ELSE lcDefParam = REPLACE(lcDefParam,"#ADDBUNDLE","").
   END.

   IF INDEX(lcDefParam,"#CLITYPE#") > 0 THEN DO:

      /* for family tariffs */
      IF icDCEvent BEGINS "CONTF" OR
         icDCEvent BEGINS "CONTS" THEN lcTagValue = icDCEvent.
      ELSE IF (icCLIType = "CONTF" OR icCLIType = "CONTS" OR
         icCLIType = "CONTSF") AND
         INDEX(lcDefParam,"#UPSELL") = 0 THEN
         lcTagValue = bContSub.TariffBundle.
      ELSE lcTagValue = "".

      FOR FIRST CTServel NO-LOCK WHERE
                CTServel.Brand = gcBrand AND
                CTServel.ServCom = "SHAPER_STP" AND
                CTServel.CLIType = icCLIType AND
                CTServel.FromDate <= idtDate:
         ASSIGN 
             lcTagValue = CTServel.DefParam
             lcOrgTagValue = lcTagValue.
      END.

      IF lcTagValue > "" THEN lcTagValue = lcTagValue + "w".

      IF lcTagValue > "" AND INDEX(icDCevent,"RELAX") > 0 AND BUFFER-TENANT-NAME(bContSub) = "TMasmovil" THEN 
      DO:
          FOR EACH bBundleAct NO-LOCK WHERE bBundleAct.MsSeq    = iiMsSeq           AND
                                            bBundleAct.DialType = {&DIAL_TYPE_GPRS} AND
                                            bBundleAct.EndTS    > ldeEndTime,
              FIRST bBundleLimit NO-LOCK WHERE bBundleLimit.SlSeq     = bBundleAct.SlSeq AND
                                               bBundleLimit.GroupCode NE icDCEvent       AND
                                               bBundleLimit.GroupCode NE "MM_DATA600",
              FIRST bContract NO-LOCK WHERE bContract.Brand   = gcBrand                AND
                                            bContract.DCEvent = bBundleLimit.GroupCode AND
                                            LOOKUP(bContract.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0,
              FIRST bDCPackage NO-LOCK WHERE bDCPackage.Brand     = gcBrand           AND
                                             bDCPackage.DCEvent   = bContract.DCEvent AND
                                             bDCPackage.ServPac   = "SHAPER"          AND
                                             bDCPackage.ToDate   >= idtDate           AND
                                             bDCPackage.FromDate <= idtDate,
              FIRST bDCComponent NO-LOCK WHERE bDCComponent.DCServicePackageID = bDCPackage.DCServicePackageID AND
                                               bDCComponent.ServCom = "SHAPER"  AND
                                               bDCComponent.ToDate   >= idtDate AND
                                               bDCComponent.FromDate <= idtDate:

              IF bDCComponent.DefParam = ""                   OR 
                 INDEX(bDCComponent.DefParam,"#CLITYPE#") = 0 OR 
                 CAN-FIND(FIRST MsRequest NO-LOCK WHERE MsRequest.MsSeq      = bBundleAct.MsSeq                          AND 
                                                        MsRequest.ReqType    = {&REQTYPE_CONTRACT_TERMINATION}           AND 
                                                        MsRequest.ReqCParam3 = bContract.DCEvent                         AND
                                                        LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0 AND
                                                         MsRequest.ActStamp   < ldeActTime) THEN
                  NEXT.

              ASSIGN lcTagValue = lcTagValue + REPLACE(bDCComponent.DefParam,"#CLITYPE#","") + "w". 
              LEAVE.                                
          END.                                     
      END.

      lcDefParam = REPLACE(lcDefParam,"#CLITYPE#",lcTagValue).
   END.

   return lcDefParam.

END FUNCTION.

&ENDIF
