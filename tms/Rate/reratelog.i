/* reratelog.i     16.03.12/aam
*/

{commali.i}
{timestamp.i}

DEF VAR lrRerateLog AS ROWID NO-UNDO.

FUNCTION fInitializeRerateLog RETURNS LOGIC
   (iiInvCust AS INT,
    iiMsSeq   AS INT,
    icCLI     AS CHAR,
    icSource  AS CHAR,
    idaFrom   AS DATE,
    idaTo     AS DATE):
    
   CREATE RerateLog.
   ASSIGN 
       RerateLog.Brand   = gcBrand
       RerateLog.InvCust = iiInvCust
       RerateLog.MsSeq   = iiMsSeq
       RerateLog.CLI     = icCli
       RerateLog.EventSource = icSource
       RerateLog.PeriodBegin = idaFrom
       RerateLog.PeriodEnd   = idaTo
       RerateLog.Started     = fMakeTS()
       RerateLog.StartDate   = TODAY
       lrRerateLog = ROWID(RerateLog).
      
   RETURN TRUE.
        
END FUNCTION.
    
FUNCTION fFinalizeRerateLog RETURNS LOGIC
   (iiChangedQty AS INT):
   
   FIND FIRST RerateLog WHERE ROWID(RerateLog) = lrRerateLog 
      EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE RerateLog THEN ASSIGN 
      RerateLog.Ended = fMakeTS()
      RerateLog.ChangedQty = iiChangedQty.
         
   RETURN (AVAILABLE RerateLog).
      
END FUNCTION.

