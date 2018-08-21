USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonConstruct.
USING OpenEdge.Core.WidgetHandle.
USING OpenEdge.Core.String.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Class.Logger FINAL:

   CONSTRUCTOR PRIVATE Logger():
   END CONSTRUCTOR.

   METHOD PUBLIC STATIC LONGCHAR objToLongChar
      ( ioObject AS CLASS Progress.Lang.Object ):

      DEFINE VARIABLE lcLongChar AS LONGCHAR NO-UNDO.
      DEFINE VARIABLE lhXmlDoc   AS HANDLE   NO-UNDO. 

      IF NOT VALID-OBJECT(ioObject)
      THEN RETURN "".

      IF TYPE-OF(ioObject, JsonConstruct)
      THEN CAST(ioObject, JsonConstruct):Write(INPUT-OUTPUT lcLongChar).

      ELSE IF TYPE-OF(ioObject, WidgetHandle)
      THEN DO:
         lhXmlDoc = CAST(ioObject, WidgetHandle):Value.
         lhXmlDoc:SAVE("LONGCHAR", lcLongChar).
      END. 

      ELSE DO:
          IF TYPE-OF(ioObject, String) THEN
             lcLongChar = CAST(ioObject, String):Value.
          ELSE
             lcLongChar = ioObject:ToString().
      END.

      RETURN lcLongChar.

   END METHOD.

   METHOD PUBLIC STATIC VOID writeLog
      ( iiLevel  AS INTEGER,
        ioObject AS CLASS Progress.Lang.Object,
        icType   AS CHARACTER ):

      IF LOG-MANAGER:LOGGING-LEVEL < iiLevel OR
         LOG-MANAGER:LOGFILE-NAME  = ?
      THEN RETURN.

      writeLog(iiLevel, objToLongChar(ioObject), icType).

   END METHOD.

   METHOD PUBLIC STATIC VOID writeLog
      ( iiLevel AS INTEGER,
        iclMsg  AS LONGCHAR,
        icType  AS CHARACTER ):

      IF LOG-MANAGER:LOGGING-LEVEL < iiLevel OR
         LOG-MANAGER:LOGFILE-NAME  = ?
      THEN RETURN.

      IF NOT iclMsg > ""
      THEN RETURN.

      IF LENGTH(iclMsg) > 30000
      THEN DO:
         DEFINE VARIABLE lii      AS INTEGER NO-UNDO.
         DEFINE VARIABLE liLength AS INTEGER NO-UNDO.
         liLength = LENGTH(iclMsg).
         DO lii = 1 TO liLength BY 30000:
            LOG-MANAGER:WRITE-MESSAGE(STRING(SUBSTRING(iclMsg, lii, 30000)), icType).
         END.
      END.
      ELSE LOG-MANAGER:WRITE-MESSAGE(STRING(iclMsg), icType).

   END METHOD.
   
   METHOD PUBLIC STATIC VOID writeLog
      ( iiLevel AS INTEGER,
        icMsg   AS CHARACTER,
        icType  AS CHARACTER ):

      IF LOG-MANAGER:LOGGING-LEVEL < iiLevel OR
         LOG-MANAGER:LOGFILE-NAME  = ?
      THEN RETURN.

      IF NOT icMsg > ""
      THEN RETURN.

      LOG-MANAGER:WRITE-MESSAGE(icMsg, icType).

   END METHOD.

END CLASS.
