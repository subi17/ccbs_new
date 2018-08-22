 
 /*------------------------------------------------------------------------
    File        : ProCommandDSS
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
USING Gwy.SAPC.ProCommand.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.ProCommandDSS INHERITS ProCommand: 

   CONSTRUCTOR ProCommandDSS
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).

      IF MsRequest.ReqType NE {&REQTYPE_DSS}
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("ProCommandDSS can only " +
                                               "handle ReqType &1",
                                               {&REQTYPE_DSS})).

      acProCommandTarget = STRING(Gwy.SAPC.TargetEnum:NB_AS).

      CASE MsRequest.ReqCParam1:

         WHEN "DELETE" THEN mDeleteDSS().

         WHEN "REMOVE" THEN mRemoveDSS().

         WHEN "ADD" THEN mAddDSS().

         WHEN "CREATE" THEN mCreateDSS().

         WHEN "MODIFY" THEN DO:

            IF INDEX(MsRequest.ReqCParam2, "QUOTA") > 0
            THEN mCreateUpsellDSS().

            ELSE IF INDEX(MsRequest.ReqCParam2, "LIMIT") > 0
            THEN mModifyDSS().

            ELSE UNDO, THROW NEW AppError(SUBSTITUTE("ProCommandDSS encourters msrequest &1 " +
                                                     "which it is unable to handle",
                                                     aiMsRequest)).
         END.
      END CASE.

   END CONSTRUCTOR.

   METHOD PRIVATE VOID mCreateUpsellDSS():

      DEFINE BUFFER DayCampaign FOR DayCampaign.
      DEFINE VARIABLE loJson    AS CLASS JsonObject NO-UNDO.

      FIND DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = "1" AND
           DayCampaign.DCEvent = MsRequest.ReqCParam3
      NO-ERROR.

      IF NOT AVAILABLE DayCampaign
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find upsell '&1'",
                                               MsRequest.ReqCParam3)).

      IF NOT DayCampaign.EMACode > ""
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Upsell &1 doesn't have EMAcode",
                                               MsRequest.ReqCParam3)).

      ASSIGN
         loJson                = NEW JsonObject()
         acProCommandType      = "CREATE_UPSELL_DSS"
         acProCommandVerb      = "PUT"
         acProCommandtargetURL = SUBSTITUTE("/groups/&1/upsells&2",
                                            MsRequest.CustNum,
                                            DayCampaign.EMACode).

      loJson:Add("priority", "1").

      FINALLY:
         IF VALID-OBJECT(loJson)
         THEN aoCommandLine = loJson.
      END FINALLY.

   END METHOD.


   METHOD PRIVATE DECIMAL mGetModifyAmount():

      DEFINE BUFFER ShaperConf FOR ShaperConf.
      DEFINE BUFFER ServiceLimit FOR ServiceLimit.
      DEFINE BUFFER MServiceLimit FOR MServiceLimit.

      DEFINE VARIABLE ldeActStamp AS DECIMAL NO-UNDO.

      IF MSRequest.ReqCparam3 EQ {&DSS4}
      THEN DO:
         FIND FIRST ShaperConf NO-LOCK WHERE
                    ShaperConf.Brand        EQ "1" AND
                    ShaperConf.ShaperConfID EQ {&DSS4SHAPERID}
         NO-ERROR.
         IF NOT AVAILABLE ShaperConf
         THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find shaperconf '&1'",
                                                  {&DSS4SHAPERID})).
         RETURN ShaperConf.LimitUnshaped / 1048576.
      END.

      ldeActStamp = IF MsRequest.ActStamp > Func.Common:mMakeTS()
                    THEN MsRequest.ActStamp
                    ELSE Func.Common:mMakeTS().

      FOR EACH ServiceLimit NO-LOCK WHERE
               ServiceLimit.GroupCode BEGINS "DSS":

         IF ServiceLimit.GroupCode MATCHES "*UPSELL*"
         THEN NEXT.

         FOR EACH MServiceLimit NO-LOCK WHERE
                  MServiceLimit.CustNum  = MsRequest.CustNum     AND
                  MServiceLimit.DialType = ServiceLimit.DialType AND
                  MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
                  MServiceLimit.EndTS   >= ldeActStamp:

            RETURN MServiceLimit.InclAmt.
         END.
      END.

      UNDO, THROW NEW AppError("DSS modify limit not available").

   END METHOD.

   METHOD PRIVATE VOID mModifyDSS():

      DEFINE VARIABLE loJson    AS CLASS JsonObject NO-UNDO.

      ASSIGN
         loJson                = NEW JsonObject()
         acProCommandType      = "UPDATE_DSS_GROUP_DATALIMIT"
         acProCommandVerb      = "POST"
         acProCommandtargetURL = SUBSTITUTE("/groups/&1/set-accumulated-volume",
                                            MsRequest.CustNum).

      loJson:Add("priority", "1000"). /* For Base tariff = 1000 */
      loJson:Add("totalDataAmount", STRING(mGetModifyAmount())).

      FINALLY:
         IF VALID-OBJECT(loJson)
         THEN aoCommandLine = loJson.
      END FINALLY.

   END METHOD.


   METHOD PRIVATE VOID mCreateDSS():

      DEFINE BUFFER MobSub FOR MobSub.
      DEFINE BUFFER CLIType FOR CLIType.

      DEFINE VARIABLE loJson    AS CLASS JsonObject NO-UNDO.

      FIND MobSub NO-LOCK WHERE
          MobSub.MsSeq = aiMsSeq
      NO-ERROR.

      IF NOT AVAILABLE MobSub
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find mobsub &1 for msrequest &2",
                                               aiMsSeq, MsRequest.MsRequest)).

      FIND CLIType NO-LOCK WHERE
           CLIType.Brand = "1" AND
           CLIType.CLIType = MobSub.CLIType
      NO-ERROR.

      IF NOT AVAILABLE CLIType
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find clitype &1 for mobsub &2",
                                               MobSub.CLIType, aiMsSeq)).

      ASSIGN
         loJson                = NEW JsonObject()
         acProCommandType      = "CREATE_DSS_GROUP"
         acProCommandVerb      = "PUT"
         acProCommandtargetURL = SUBSTITUTE("/groups/&1", MsRequest.CustNum).

      loJson:Add("type", IF CLIType.PayType EQ 2 THEN "prepaid" ELSE "postpaid").
      loJson:Add("priority", "1000"). /* For Base tariff = 1000 */
      loJson:Add("totalDataAmount", mToMB(mGetValue(MSRequest.ReqCParam2, "LIMIT_UNSHAPED"))).

      mAddMSISDN(MSRequest.ReqCParam2, loJson).

      FINALLY:
         IF VALID-OBJECT(loJson)
         THEN aoCommandLine = loJson.
      END FINALLY.

   END METHOD.

   METHOD PRIVATE CHARACTER mToMB
      (icBytes AS CHARACTER):

      RETURN STRING(INT64(icBytes) / 1048576).

   END METHOD.

   METHOD PRIVATE VOID mDeleteDSS():
      ASSIGN
         acProCommandType      = "TERMINATE_DSS_GROUP"
         acProCommandVerb      = "DELETE"
         acProCommandtargetURL = SUBSTITUTE("/groups/&1",
                                            MsRequest.CustNum).
   END METHOD.

   METHOD PRIVATE VOID mRemoveDSS():

      DEFINE VARIABLE loJson AS CLASS JsonObject NO-UNDO.

      ASSIGN
         loJson                = NEW JsonObject()
         acProCommandType      = "REMOVE_FROM_DSS_GROUP"
         acProCommandVerb      = "POST"
         acProCommandtargetURL = SUBSTITUTE("/groups/&1/manage-subscription",
                                            MsRequest.CustNum).

      loJson:Add("action", "remove").

      mAddMSISDN(MSRequest.ReqCParam2, loJson).

      FINALLY:
         IF VALID-OBJECT(loJson)
         THEN aoCommandLine = loJson.
      END FINALLY.

   END METHOD.

   METHOD PRIVATE VOID mAddDSS():

      DEFINE VARIABLE loJson AS CLASS JsonObject NO-UNDO.

      ASSIGN
         loJson                = NEW JsonObject()
         acProCommandType      = "ADD_TO_DSS_GROUP" 
         acProCommandVerb      = "POST"
         acProCommandtargetURL = SUBSTITUTE("/groups/&1/manage-subscription",
                                            MsRequest.CustNum).

      loJson:Add("action", "add").

      mAddMSISDN(MSRequest.ReqCParam2, loJson).

      FINALLY:
         IF VALID-OBJECT(loJson)
         THEN aoCommandLine = loJson.
      END FINALLY.

   END METHOD.

   METHOD PRIVATE CHARACTER mGetValue
      ( icText AS CHARACTER,
        icKey  AS CHARACTER ):

      DEFINE VARIABLE liKeyPos AS INTEGER NO-UNDO.
      DEFINE VARIABLE liEndPos AS INTEGER NO-UNDO.

      ASSIGN
         icKey = icKey + "="
         liKeyPos = INDEX(icText,icKey).

      IF liKeyPos EQ 0
      THEN RETURN "".

      ASSIGN
         liKeyPos = liKeyPos + LENGTH(icKey)
         liEndPos = INDEX(icText,",",liKeyPos).

      IF liEndPos EQ 0
      THEN RETURN SUBSTRING(icText, liKeyPos).

      RETURN SUBSTRING(icText, liKeyPos, liEndPos - liKeyPos).

   END METHOD.

   METHOD PRIVATE VOID mAddMSISDN
      ( icParamText  AS CHARACTER,
        ioJsonObject AS CLASS JsonObject ):

      DEFINE VARIABLE lcMSISDN    AS CHARACTER          NO-UNDO.
      DEFINE VARIABLE loJsonArray AS CLASS JsonArray    NO-UNDO.
      DEFINE VARIABLE lii         AS INTEGER            NO-UNDO.

      lcMSISDN = mGetValue(icParamText,"MSISDNS").
      IF lcMSISDN EQ ""
      THEN lcMSISDN = mGetValue(icParamText,"MSISDN").

      IF NOT lcMSISDN > ""
      THEN RETURN.

      IF NUM-ENTRIES(lcMSISDN, ";") > 1
      THEN DO:
         loJsonArray = NEW JsonArray().

         DO lii = 1 TO NUM-ENTRIES(lcMSISDN, ";"):
            loJsonArray:Add(ENTRY(lii,lcMSISDN, ";")).
         END.

         ioJsonObject:Add("msisdns", loJsonArray).
      END.

      ELSE ioJsonObject:Add("msisdn", lcMSISDN).

   END METHOD.

END CLASS.