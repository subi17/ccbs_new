 /*------------------------------------------------------------------------
    File        : MManMessage
    Purpose     : 
    Syntax      : 
    Description : Creates MMLog records. They are created using a
                  method mCreateMessage
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonObject.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Mm.MManMessage FINAL: 

   CONSTRUCTOR STATIC MManMessage():

   END CONSTRUCTOR.


   METHOD PRIVATE STATIC CHARACTER mLanguageCode
      ( iiLanguageID AS INTEGER ):

      FOR Language NO-LOCK WHERE
         Language.Language = iiLanguage:
         RETURN Language.LanguageCode.
      END.

      RETURN "ES".

   END METHOD.


   METHOD PRIVATE STATIC CHARACTER mConvertToJson
      ( icParamKeys   AS CHARACTER,
        icParamValues AS CHARACTER ):

      IF NUM-ENTRIES(icParamKeys, CHR(14)) NE NUM-ENTRIES(icParamValues, CHR(14))
      THEN RETURN ERROR NEW Progress.Lang.AppError("Param key entry count doesn't match to param value entry count!", 1).

      IF icParamKeys = ""
      THEN RETURN.

      DEFINE VARIABLE objJsonObject AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE lii AS INTEGER NO-UNDO.

      objJsonObject = NEW JsonObject().

      DO lii = 1 TO NUM-ENTRIES(icParamKeys, CHR(14)):
         objJsonObject:Add(ENTRY(lii,icParamKeys,CHR(14)),ENTRY(lii,lcParamValues,CHR(14))).
      END.

      objJsonObject:Write(OUTPUT lcJson).

      RETURN lcJson.

      FINALLY:
         IF VALID-OBJECT(objJsonObject)
         THEN DELETE OBJECT objJsonObject.
      END FINALLY.

   END METHOD.


   METHOD PUBLIC STATIC VOID mCreateMessage
      ( icTarget         AS CHARACTER,
        icKeyValue       AS CHARACTER,
        iiLanguage       AS INTEGER,
        icSMSRecipient   AS CHARACTER,
        icEmailRecipient AS CHARACTER,
        icPushRecipient  AS CHARACTER,
        icParamValues    AS CHARACTER ): /* CHR(14) separated list */

      DO WHILE TRUE:

         FIND LAST InvText NO-LOCK WHERE
            InvText.Brand    = "1"        AND
            InvText.Target   = icTarget   AND
            InvText.KeyValue = icKeyValue AND
            InvText.FromDate <= TODAY     AND
            InvText.Language = iiLanguage
         NO-ERROR.

         IF NOT AVAILABLE InvText AND iiLanguage NE 1
         THEN iiLanguage = 1.
         ELSE LEAVE.

      END.

      IF NOT AVAILABLE InvText
      THEN RETURN ERROR NEW Progress.Lang.AppError("InvText with Target " + icTarget + " and KeyValue " + icKeyValue + " doesn't exist!", 1).

      CREATE MMLog.
      ASSIGN
         MMLog.RequestID          = "706" + GUID(GENERATE-UUID)
         MMLog.BrandName          = multitenancy.TenantInformation:mGetBrandForTable("InvText")
         MMLog.Originator         = PROGRAM-NAME(2) WHEN PROGRAM-NAME(2) NE ?
         MMLog.SMSRecipient       = icSMSRecipient
         MMLog.EmailRecipient     = icEmailRecipient
         MMLog.PushRecipient      = icPushRecipient
         MMLog.TemplateId         = InvText.TemplateID
         MMLog.Language           = mLanguageCode(iiLanguage)
         MMLog.MessageBody        = InvText.InvText WHEN InvText.TemplateID = "" 
         MMLog.Category           = InvText.Category
         MMLog.SchedulingPolicy   = InvText.ShedulingPolicy
         MMLog.SchedulingPriority = InvText.SchedulingPriority
         MMLog.CallBacks          = InvText.CallBacks
         MMLog.JsonParam          = mConvertToJson(InvText.ParamKeys,icParamValues)
         MMLog.RequestTime        = NOW
         .

   END METHOD.        

END CLASS.