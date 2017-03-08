/* ----------------------------------------------------------------------
  MODULE .......: air_update_serviceclass.p
  TASK .........: Creates UpdateServiceClass query to AIR to update 
                  TARJ5 serviceclass
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 18.01.12
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commali.i}
{Func/date.i}
{Gwy/air.i}

DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER piServiceClass AS INT NO-UNDO.
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
   DEF VAR liFieldCount AS INT NO-UNDO. 

   IF piServiceClassTemp NE 0 AND piServiceClassTemp NE ? THEN
      liFieldCount = (IF pdaExpiryDate NE ? THEN 9 ELSE 8).
   ELSE 
      liFieldCount = 7.

   DO liLoop = 1 TO liFieldCount:
      
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
            ttUCIP.ttValue  = "Set"
            ttUCIP.ttFormat = "string".
         WHEN 7 THEN ASSIGN
            ttUCIP.ttName   = "serviceClassNew"
            ttUCIP.ttValue  = STRING(piServiceClass)
            ttUCIP.ttFormat = "int".
         WHEN 8 THEN ASSIGN
            ttUCIP.ttName   = "serviceClassTemporaryNew"
            ttUCIP.ttValue  = STRING(piServiceClassTemp)
            ttUCIP.ttFormat = "int".
         WHEN 9 THEN ASSIGN
            ttUCIP.ttName   = "serviceClassTemporaryNewExpiryDate"
            ttUCIP.ttValue  = fISO8601Date(pdaExpiryDate)
            ttUCIP.ttFormat = "dateTime.iso8601".

      END.

   END.

END PROCEDURE.
