/*------------------------------------------------------------------------
    File        : hpd_filedump.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Oct 27 12:27:11 EET 2014
    Notes       :
  ----------------------------------------------------------------------*/

{Syst/commali.i}

DEFINE INPUT  PARAMETER iiDumpID      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER icFile        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icDumpMode    AS CHARACTER NO-UNDO. /* NOT USED */
DEFINE INPUT  PARAMETER idLastDump    AS DECIMAL   NO-UNDO. /* NOT USED */
DEFINE INPUT  PARAMETER icEventSource AS CHARACTER NO-UNDO. /* NOT USED */
DEFINE INPUT  PARAMETER icEventFields AS CHARACTER NO-UNDO. /* NOT USED */
DEFINE OUTPUT PARAMETER oiEvents      AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER olInterrupted AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lii              AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcReturnValue    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcClassName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE llInactivateDump AS LOGICAL   INITIAL NO NO-UNDO.

DEFINE VARIABLE HandlerObj AS CLASS HPD.DumpHandler NO-UNDO.

FIND FIRST DumpFile NO-LOCK WHERE
   DumpFile.DumpID = iiDumpID
NO-ERROR.

IF NOT AVAILABLE DumpFile
THEN RETURN "ERROR: DumpFile record doesn't exist". 

IF NOT DumpFile.FileCategory = "HPD"
THEN RETURN "ERROR: Only HPD type dumpfile is allowed".

lcClassName = DumpFile.LinkKey.

IF NOT lcClassName BEGINS "HPD."
THEN RETURN "ERROR: DumpFile LinkKey field doesn't contain valid HPD class".

HandlerObj = DYNAMIC-NEW lcClassName(iiDumpID, icFile).

ASSIGN
   oiEvents      = HandlerObj:liEvents
   olInterrupted = HandlerObj:llInterrupted
   .

CATCH apperrorobj AS Progress.Lang.AppError:
   
   IF apperrorobj:ReturnValue <> "" AND apperrorobj:ReturnValue <> ?
   THEN RETURN "ERROR: " + apperrorobj:ReturnValue. 
   
   lcReturnValue = "ERROR: ".
   DO lii = 1 TO apperrorobj:NumMessages:    
        lcReturnValue = lcReturnValue + " " + apperrorObj:GetMessage(lii).
   END.

   RETURN lcReturnValue.   

END.

CATCH errorobj AS Progress.Lang.ProError:
 
   lcReturnValue = "ERROR: ".
   DO lii = 1 TO errorobj:NumMessages:  
      
      IF errorObj:GetMessage(lii) BEGINS "DYNAMIC-NEW cannot instantiate class"
      THEN ASSIGN
              llInactivateDump = YES         
              lcReturnValue = lcReturnValue + " " + "Class " + lcClassName + " doesn't allow offline HPD handling. The dump is automatically set to inactive state."
              .
      ELSE lcReturnValue = lcReturnValue + " " + errorObj:GetMessage(lii).
   END.

   RETURN lcReturnValue.   
     
END.

FINALLY:

   IF VALID-OBJECT(HandlerObj)
   THEN DELETE OBJECT HandlerObj.
   
   IF llInactivateDump
   THEN DO TRANSACTION:
      
      FIND FIRST DumpFile EXCLUSIVE-LOCK WHERE
         DumpFile.DumpID = iiDumpID
      NO-ERROR.
      
      IF AVAILABLE DumpFile
      THEN DumpFile.Active = NO.

      RELEASE DumpFile.       
   
   END.
   
END FINALLY.
