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
{Func/multitenantfunc.i}

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

LOG-MANAGER:LOGFILE-NAME = "../var/log/hpd_filedump_" + STRING(iiDumpID) + 
                           "_" + fGetTableBrand("DFTimeTable") + ".log".

FIND FIRST DumpFile NO-LOCK WHERE
   DumpFile.DumpID = iiDumpID
NO-ERROR.

IF NOT AVAILABLE DumpFile
THEN DO:
   LOG-MANAGER:WRITE-MESSAGE("DumpFile record doesn't exist", "ERROR").
   RETURN "ERROR: DumpFile record doesn't exist".
END.

IF NOT DumpFile.FileCategory = "HPD"
THEN DO:
   LOG-MANAGER:WRITE-MESSAGE("Only HPD type dumpfile is allowed", "ERROR").
   RETURN "ERROR: Only HPD type dumpfile is allowed".
END.

lcClassName = DumpFile.LinkKey.

IF NOT lcClassName BEGINS "HPD."
THEN DO:
   LOG-MANAGER:WRITE-MESSAGE("DumpFile LinkKey field doesn't contain valid HPD class", "ERROR").
   RETURN "ERROR: DumpFile LinkKey field doesn't contain valid HPD class".
END.

LOG-MANAGER:WRITE-MESSAGE("Start processing dump " + DumpFile.DumpName, "INFO").

HandlerObj = DYNAMIC-NEW lcClassName(iiDumpID, icFile).

ASSIGN
   oiEvents      = HandlerObj:liEvents
   olInterrupted = HandlerObj:llInterrupted
   .

IF HandlerObj:llInterrupted
THEN DO:
   LOG-MANAGER:WRITE-MESSAGE("Process was interrupted when " + STRING(HandlerObj:liEvents) + " was dumped", "INFO").
   RETURN.
END.
ELSE LOG-MANAGER:WRITE-MESSAGE("Completed the dump. " + STRING(HandlerObj:liEvents) + " rows was dumped.", "INFO").

CATCH apperrorobj AS Progress.Lang.AppError:
   
   IF apperrorobj:ReturnValue <> "" AND apperrorobj:ReturnValue <> ?
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE(apperrorobj:ReturnValue, "ERROR").
      RETURN "ERROR: " + apperrorobj:ReturnValue.
   END.
   
   DO lii = 1 TO apperrorobj:NumMessages:    
        lcReturnValue = lcReturnValue + " " + apperrorObj:GetMessage(lii).
   END.

   LOG-MANAGER:WRITE-MESSAGE(lcReturnValue, "ERROR").

   RETURN "ERROR: " + lcReturnValue.

END.

CATCH errorobj AS Progress.Lang.ProError:
 
   DO lii = 1 TO errorobj:NumMessages:  
      
      IF errorObj:GetMessage(lii) BEGINS "DYNAMIC-NEW cannot instantiate class"
      THEN ASSIGN
              llInactivateDump = YES         
              lcReturnValue = lcReturnValue + " " + "Class " + lcClassName + " doesn't allow offline HPD handling. The dump is automatically set to inactive state."
              .
      ELSE lcReturnValue = lcReturnValue + " " + errorObj:GetMessage(lii).
   END.

   LOG-MANAGER:WRITE-MESSAGE(lcReturnValue, "ERROR").

   RETURN "ERROR: " + lcReturnValue.
     
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
