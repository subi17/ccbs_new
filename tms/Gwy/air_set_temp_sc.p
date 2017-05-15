/* ----------------------------------------------------------------------
  MODULE .......: air_set_temp_sc.p
  TASK .........: Set temporary SC class
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 16.04.14
  Version ......: Yoigo
----------------------------------------------------------------------- */
{Syst/commali.i}
{Func/date.i}
{Gwy/air.i}

DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER piServiceClassTemp AS INT NO-UNDO.
DEFINE INPUT PARAMETER pdaExpiryDate AS DATE NO-UNDO.
DEF OUTPUT PARAM ocError AS CHAR NO-UNDO.

DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE liResponseCode AS INT NO-UNDO.

RUN pHeader(pcCLI).
RUN pPrePaidPlatform("UpdateServiceClass").

lcResponse = RETURN-VALUE.

liResponseCode = INT(fGetRPCNodeValue(lcResponse,"responseCode")) NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
   ocError = SUBST("ERROR:AIR UpdateServiceClass response: &1", lcResponse).
   RETURN.
END.

IF liResponseCode NE 0 THEN DO:
   ocError = "ERROR:AIR UpdateServiceClass responseCode: " +
            STRING(liResponseCode).
   RETURN.
END.

RETURN.

PROCEDURE pHeader:

   DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.

   DEF VAR liLoop AS INTEGER NO-UNDO.

   DO liLoop = 1 TO 8:

      CREATE ttUCIP.

      CASE liLoop:
         WHEN 1 THEN ASSIGN
            ttUCIP.ttName   = "originNodeType"
            ttUCIP.ttValue  = "AIR"
            ttUCIP.ttFormat = "string".
         WHEN 2 THEN ASSIGN
            ttUCIP.ttName   = "originHostName"
            ttUCIP.ttValue  = "propus"
            ttUCIP.ttFormat = "string".
         WHEN 3 THEN ASSIGN
            ttUCIP.ttName   = "originTransactionID"
            ttUCIP.ttValue  = STRING(NEXT-VALUE(PrePaidReq),"999999999")
            ttUCIP.ttFormat = "string".
         WHEN 4 THEN ASSIGN
            ttUCIP.ttValue  = fISO860(fMakeTS())
            ttUCIP.ttName   = "originTimeStamp"
            ttUCIP.ttFormat = "dateTime.iso8601".
         WHEN 5 THEN ASSIGN
            ttUCIP.ttName   = "subscriberNumber"
            ttUCIP.ttValue  = pcCLI
            ttUCIP.ttFormat = "string".
         WHEN 6 THEN ASSIGN
            ttUCIP.ttName   = "serviceClassAction"
            ttUCIP.ttValue  = "SetTemporary"
            ttUCIP.ttFormat = "string".
         WHEN 7 THEN ASSIGN
            ttUCIP.ttName   = "serviceClassTemporaryNew"
            ttUCIP.ttValue  = STRING(piServiceClassTemp)
            ttUCIP.ttFormat = "int".
         WHEN 8 THEN ASSIGN
            ttUCIP.ttName   = "serviceClassTemporaryNewExpiryDate"
            ttUCIP.ttValue  = fISO8601Date(pdaExpiryDate)
            ttUCIP.ttFormat = "dateTime.iso8601".

      END.

   END.

END PROCEDURE.
