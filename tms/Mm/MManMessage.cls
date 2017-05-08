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

   DEFINE PRIVATE STATIC VARIABLE acBrandName          AS CHARACTER NO-UNDO.
   DEFINE PRIVATE STATIC VARIABLE acTemplateId         AS CHARACTER NO-UNDO.
   DEFINE PRIVATE STATIC VARIABLE acLanguage           AS CHARACTER NO-UNDO.
   DEFINE PRIVATE STATIC VARIABLE acMessageBody        AS CHARACTER NO-UNDO.
   DEFINE PRIVATE STATIC VARIABLE acCategory           AS CHARACTER NO-UNDO.
   DEFINE PRIVATE STATIC VARIABLE acSchedulingPolicy   AS CHARACTER NO-UNDO.
   DEFINE PRIVATE STATIC VARIABLE aiSchedulingPriority AS INTEGER   NO-UNDO.
   DEFINE PRIVATE STATIC VARIABLE acCallBacks          AS CHARACTER NO-UNDO.


   DEFINE PUBLIC STATIC PROPERTY ParamKeyValue  AS CHARACTER NO-UNDO
      GET.
      PRIVATE SET.

   METHOD PRIVATE STATIC CHARACTER mLanguageCode
      ( iiLanguageID AS INTEGER ):

      DEFINE BUFFER Language FOR Language.

      FOR Language NO-LOCK WHERE
         Language.Language = iiLanguageID:
         RETURN Language.LanguageCode.
      END.

      RETURN "ES".

   END METHOD.


   METHOD PRIVATE STATIC CHARACTER mConvertToJson
      ( icParamKeyValue AS CHARACTER ):

      IF icParamKeyValue = ""
      THEN RETURN "".

      DEFINE VARIABLE objJsonObject AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE lcEntry       AS CHARACTER        NO-UNDO.
      DEFINE VARIABLE lcJson        AS CHARACTER        NO-UNDO.
      DEFINE VARIABLE lii           AS INTEGER          NO-UNDO.

      objJsonObject = NEW JsonObject().

      DO lii = 1 TO NUM-ENTRIES(icParamKeyValue, "|"):
         lcEntry = ENTRY(lii,icParamKeyValue,"|").
         IF NUM-ENTRIES(lcEntry,"=") NE 2
         THEN RETURN ERROR NEW Progress.Lang.AppError("Invalid param key value " + lcEntry, 1).
         objJsonObject:Add(ENTRY(1,lcEntry,"="),ENTRY(2,lcEntry,"=")).
      END.

      objJsonObject:Write(OUTPUT lcJson).

      RETURN lcJson.

      FINALLY:
         IF VALID-OBJECT(objJsonObject)
         THEN DELETE OBJECT objJsonObject.
      END FINALLY.

   END METHOD.


   METHOD PRIVATE STATIC VOID mClearData():

      ASSIGN
         acBrandName          = ""
         acTemplateId         = ""
         ParamKeyValue        = ""
         acLanguage           = ""
         acMessageBody        = ""
         acCategory           = ""
         acSchedulingPolicy   = ""
         aiSchedulingPriority = 0
         acCallBacks          = ""
         .

   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mGetMessage
      ( icTarget   AS CHARACTER,
        icKeyValue AS CHARACTER,
        iiLanguage AS INTEGER ):

      DEFINE BUFFER InvText FOR InvText.

      mClearData().

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
      THEN DO:
         IF LOG-MANAGER:LOGFILE-NAME > ""
         THEN LOG-MANAGER:WRITE-MESSAGE("InvText with Target " + icTarget + " and KeyValue " + icKeyValue + " doesn't exist!", "ERROR").
         RETURN FALSE.
      END.

      IF InvText.UseMMan
      THEN ASSIGN
              acBrandName          = multitenancy.TenantInformation:mGetBrandForTable("InvText")
              acTemplateId         = InvText.TemplateID
              ParamKeyValue        = InvText.ParamKeyValue
              acLanguage           = mLanguageCode(iiLanguage)
              acMessageBody        = InvText.InvText WHEN InvText.TemplateID = ""
              acCategory           = InvText.Category
              acSchedulingPolicy   = InvText.SchedulingPolicy
              aiSchedulingPriority = InvText.SchedulingPriority
              acCallBacks          = InvText.CallBacks
              .

      RETURN InvText.UseMMan.

   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mCreateMMLog
      ( icSMSRecipient   AS CHARACTER,
        icEmailRecipient AS CHARACTER,
        icPushRecipient  AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      DEFINE BUFFER MMLog   FOR MMLog.

      CREATE MMLog.
      ASSIGN
         MMLog.RequestID          = "706" + GUID(GENERATE-UUID)
         MMLog.BrandName          = acBrandName
         MMLog.Originator         = PROGRAM-NAME(2) WHEN PROGRAM-NAME(2) NE ?
         MMLog.SMSRecipient       = icSMSRecipient
         MMLog.EmailRecipient     = icEmailRecipient
         MMLog.PushRecipient      = icPushRecipient
         MMLog.TemplateId         = acTemplateID
         MMLog.Language           = acLanguage
         MMLog.MessageBody        = acMessageBody
         MMLog.Category           = acCategory
         MMLog.SchedulingPolicy   = acSchedulingPolicy
         MMLog.SchedulingPriority = aiSchedulingPriority
         MMLog.CallBacks          = acCallBacks
         MMLog.JsonParam          = mConvertToJson(icParamValues)
         MMLog.RequestTime        = NOW
         .

      mClearData().

      RETURN TRUE.

   END METHOD.        


   METHOD PUBLIC STATIC LOGICAL mCreateMMLogSMS
      ( icSMSRecipient   AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      RETURN mCreateMessage(icSMSRecipient,
                            "",
                            "",
                            icParamValues).
   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mCreateMMLogEmail
      ( icEmailRecipient AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      RETURN mCreateMessage("",
                            icEmailRecipient,
                            "",
                            icParamValues).
   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mCreateMMLogPush
      ( icPushRecipient  AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      RETURN mCreateMessage("",
                            "",
                            icPushRecipient,
                            icParamValues).
   END METHOD.

END CLASS.