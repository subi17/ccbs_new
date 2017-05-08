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


   METHOD PUBLIC STATIC LOGICAL mGetMessageIdAndParam
      ( INPUT  icTarget        AS CHARACTER,
        INPUT  icKeyValue      AS CHARACTER,
        INPUT  iiLanguage      AS INTEGER,
        OUTPUT ocParamKeyValue AS CHARACTER,
        OUTPUT oiITNum         AS INTEGER ):

      DEFINE BUFFER InvText FOR InvText.

      ASSIGN
         oiITNum         = ?
         ocParamKeyValue = "".

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

      ASSIGN
         oiITNum         = InvText.ITNum
         ocParamKeyValue = InvText.ParamKeyValue.

      RETURN InvText.UseMMan.

   END METHOD.


   METHOD PRIVATE STATIC LOGICAL mCreateMessage
      ( iiITNum          AS INTEGER,
        icTarget         AS CHARACTER,
        icKeyValue       AS CHARACTER,
        iiLanguage       AS INTEGER,
        icSMSRecipient   AS CHARACTER,
        icEmailRecipient AS CHARACTER,
        icPushRecipient  AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      DEFINE BUFFER InvText FOR InvText.
      DEFINE BUFFER MMLog   FOR MMLog.

      IF iiITNum > 0
      THEN FIND InvText NO-LOCK WHERE InvText.ITNum = iiITNum NO-ERROR.

      ELSE DO WHILE TRUE:

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
         THEN DO:
            IF iiITNum > 0
            THEN LOG-MANAGER:WRITE-MESSAGE("InvText with ITNum " + STRING(iiITNum) + " doesn't exist!", "ERROR").
            ELSE LOG-MANAGER:WRITE-MESSAGE("InvText with Target " + icTarget + " and KeyValue " + icKeyValue + " doesn't exist!", "ERROR").
         END.
         RETURN FALSE.
      END.

      IF NOT InvText.UseMMan
      THEN RETURN FALSE.

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
         MMLog.SchedulingPolicy   = InvText.SchedulingPolicy
         MMLog.SchedulingPriority = InvText.SchedulingPriority
         MMLog.CallBacks          = InvText.CallBacks
         MMLog.JsonParam          = mConvertToJson(icParamValues)
         MMLog.RequestTime        = NOW
         .

      RETURN TRUE.

   END METHOD.        


   METHOD PUBLIC STATIC LOGICAL mCreateSMSMessage
      ( icTarget         AS CHARACTER,
        icKeyValue       AS CHARACTER,
        iiLanguage       AS INTEGER,
        icSMSRecipient   AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      RETURN mCreateMessage(?,
                            icTarget,
                            icKeyValue,
                            iiLanguage,
                            icSMSRecipient,
                            "",
                            "",
                            icParamValues).
   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mCreateSMSMessage
      ( iiITNum          AS INTEGER,
        icSMSRecipient   AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      RETURN mCreateMessage(iiITNum,
                            "",
                            "",
                            0,
                            icSMSRecipient,
                            "",
                            "",
                            icParamValues).
   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mCreateEmailMessage
      ( icTarget         AS CHARACTER,
        icKeyValue       AS CHARACTER,
        iiLanguage       AS INTEGER,
        icEmailRecipient AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      RETURN mCreateMessage(?,
                            icTarget,
                            icKeyValue,
                            iiLanguage,
                            "",
                            icEmailRecipient,
                            "",
                            icParamValues).
   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mCreateEmailMessage
      ( iiITNum          AS INTEGER,
        icEmailRecipient AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      RETURN mCreateMessage(iiITNum,
                            "",
                            "",
                            0,
                            "",
                            icEmailRecipient,
                            "",
                            icParamValues).
   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mCreatePushMessage
      ( icTarget         AS CHARACTER,
        icKeyValue       AS CHARACTER,
        iiLanguage       AS INTEGER,
        icPushRecipient  AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      RETURN mCreateMessage(?,
                            icTarget,
                            icKeyValue,
                            iiLanguage,
                            "",
                            "",
                            icPushRecipient,
                            icParamValues).
   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mCreatePushMessage
      ( iiITNum          AS INTEGER,
        icPushRecipient  AS CHARACTER,
        icParamValues    AS CHARACTER ): /* "|" separated list */

      RETURN mCreateMessage(iiITNum,
                            "",
                            "",
                            0,
                            "",
                            "",
                            icPushRecipient,
                            icParamValues).
   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mCreateMessage
      ( iiITNum          AS INTEGER,
        icSMSRecipient   AS CHARACTER,
        icEmailRecipient AS CHARACTER,
        icPushRecipient  AS CHARACTER,
        icParamValues    AS CHARACTER ):

      RETURN mCreateMessage(iiITNum,
                            "",
                            "",
                            0,
                            icSMSRecipient,
                            icEmailRecipient,
                            icPushRecipient,
                            icParamValues).

   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mCreateMessage
      ( icTarget         AS CHARACTER,
        icKeyValue       AS CHARACTER,
        iiLanguage       AS INTEGER,
        icSMSRecipient   AS CHARACTER,
        icEmailRecipient AS CHARACTER,
        icPushRecipient  AS CHARACTER,
        icParamValues    AS CHARACTER ):

      RETURN mCreateMessage(?,
                            icTarget,
                            icKeyValue,
                            iiLanguage,
                            icSMSRecipient,
                            icEmailRecipient,
                            icPushRecipient,
                            icParamValues).

   END METHOD.

END CLASS.