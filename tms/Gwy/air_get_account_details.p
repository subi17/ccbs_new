/* ----------------------------------------------------------------------
  MODULE .......: air_get_account_details.p 
  TASK .........: Get prepaid account information
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 18.01.12
  Version ......: xfera
----------------------------------------------------------------------- */
{commali.i}
{date.i}
{air.i}

DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oiServiceClassCurrent AS INT NO-UNDO.
DEF OUTPUT PARAM ocError AS CHAR NO-UNDO.

DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE liResponseCode AS INT NO-UNDO. 

RUN pHeader(pcCLI).
RUN pPrePaidPlatform("GetAccountDetails").
lcResponse = RETURN-VALUE.

liResponseCode = INT(fGetRPCNodeValue(lcResponse,"responseCode")) NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
   ocError = SUBST("ERROR:AIR GetAccountDetails response: &1", lcResponse).
   RETURN.
END.

IF liResponseCode NE 0 THEN DO:
   ocError = "ERROR:AIR GetAccountDetails responseCode: " +
            STRING(liResponseCode).
   RETURN.
END.

oiServiceClassCurrent = INT(fGetRPCNodeValue(lcResponse,"serviceClassCurrent")) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
   ocError = "ERROR:AIR GetAccountDetails response serviceClassCurrent parsing failed".
   RETURN.
END.

RETURN.

PROCEDURE pHeader:

   DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.

   DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.

   DO liLoop = 1 TO 5:
      
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
      END.

   END.

END PROCEDURE.
