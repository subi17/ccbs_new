/* -----------------------------------------------
   MODULE ........: dpl_log.i
   TASK ..........: Save log of invoice request for PDFs
   APPLICATION ...: NN
   AUTHOR ........: Janne Tourunen
   CREATED .......: 31.10.2012
   CHANGED .......:
   VERSION .......: 1.0
   FUNCTIONS .....: fSetInvoiceLogs
  ------------------------------------------------ */

{commali.i}   
{timestamp.i}

FUNCTION fCreateDPLLog RETURNS LOGICAL
  (INPUT pcUserName AS CHAR,
   INPUT pcSystemCode AS CHAR,
   INPUT pcEventType AS CHAR,
   INPUT piReasonCode AS INT,
   INPUT pcInvNum AS CHAR,
   INPUT pdeStartDate AS DATE,
   INPUT pdeEndDate AS DATE,
   INPUT pcAccessType AS CHAR,
   INPUT pcSearchRule AS CHAR):

   CREATE CallScanner.
   ASSIGN
      CallScanner.TMSTime       = fmakeTS()
      CallScanner.UserCode      = pcUserName
      CallScanner.SystemID      = pcSystemCode
      CallScanner.EventType     = pcEventType
      CallScanner.ReasonCode    = STRING(piReasonCode)
      CallScanner.Level         = ""
      CallScanner.Target        = pcInvNum
      CallScanner.StartTime     = STRING(pdeStartDate, "99/99/99") + " " +
                                  STRING("00:00:00") WHEN pdeStartDate NE ?
      CallScanner.EndTime       = STRING(pdeEndDate, "99/99/99") + " " +
                                  STRING("23:59:59") WHEN pdeEndDate NE ?
      CallScanner.AccessType    = pcAccessType
      CallScanner.SearchRule    = pcSearchRule NO-ERROR.


   RETURN TRUE.
END FUNCTION.

