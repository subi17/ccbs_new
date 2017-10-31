/* reratelog.i     16.03.12/aam
*/

{Syst/commali.i}

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
       RerateLog.Brand   = Syst.CUICommon:gcBrand
       RerateLog.InvCust = iiInvCust
       RerateLog.MsSeq   = iiMsSeq
       RerateLog.CLI     = icCli
       RerateLog.EventSource = icSource
       RerateLog.PeriodBegin = idaFrom
       RerateLog.PeriodEnd   = idaTo
       RerateLog.Started     = Func.Common:mMakeTS()
       RerateLog.StartDate   = TODAY
       lrRerateLog = ROWID(RerateLog).
      
   RETURN TRUE.
        
END FUNCTION.
    
FUNCTION fFinalizeRerateLog RETURNS LOGIC
   (iiChangedQty AS INT):
   
   FIND FIRST RerateLog WHERE ROWID(RerateLog) = lrRerateLog 
      EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE RerateLog THEN ASSIGN 
      RerateLog.Ended = Func.Common:mMakeTS()
      RerateLog.ChangedQty = iiChangedQty.
         
   RETURN (AVAILABLE RerateLog).
      
END FUNCTION.

