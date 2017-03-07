/* ----------------------------------------------------------------------
  MODULE .......: callscannerrep.p 
  TASK .........:
  APPLICATION ..: xfera 
  AUTHOR .......: anttis 
  CREATED ......: 21.11.07 - copied base code from telef
  CHANGED ......: 
  Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commali.i}
{Func/cparam2.i}
{Syst/dumpfile_run.i}
{Func/finvbal.i}
{Func/timestamp.i}
{Syst/tmsconst.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR ldStartTime AS DE   NO-UNDO.
DEF VAR ldEndTime   AS DE   NO-UNDO.
DEF VAR lcSep       AS CHAR NO-UNDO INIT ";".

FUNCTION fTutkaTime RETURN CHARACTER
(INPUT idTimeStamp AS DEC).

   DEF VAR outstring  AS C  NO-UNDO.
   DEF VAR dte        AS DA NO-UNDO.
   DEF VAR tme        AS I  NO-UNDO.

            
    fSplitTS(idTimeStamp, OUTPUT dte, OUTPUT tme) .

    outstring = STRING(Month(dte),"99")        + "/"  +
                STRING(DaY(dte),"99")         + "/"  + 
                STRING(YEAR(DTe) - 2000,"99") + " "  +
                STRING(tme,"hh:mm:ss").
   RETURN outstring.
            
END FUNCTION.

DEF STREAM Tutka.

FUNCTION fTutkaCTime RETURN CHARACTER
(INPUT icTimeStamp AS CHAR).

   DEF VAR outstring  AS C  NO-UNDO.

    outstring = SUBSTRING(icTimeStamp,4,2) + "/" +
                SUBSTRING(icTimeStamp,1,2) + 
                SUBSTRING(ictimeStamp,6).
    RETURN outstring.        
END FUNCTION.
            
             
ASSIGN
   ldStartTime = fHMS2TS(today - 1, "00:00:00").
   ldEndTime   = fHMS2TS(today,    "00:00:00") .


OUTPUT STREAM Tutka TO VALUE(icFile).

FOR EACH CallScanner NO-LOCK WHERE 
         CallScanner.TMSTime >= ldStartTime AND 
         CallScanner.TMSTime < ldEndTime.
  
  PUT STREAM Tutka Unformatted 
   CallScanner.UserCode lcSep 
   CallScanner.SystemID lcsep 
   LC(CallScanner.EventType) lcSep
   fTutkaTime(Callscanner.TMSTime) lcSep
   CallScanner.ReasonCode lcSep 
   CallScanner.Target lcSep 
   fTutkaCTime(CallScanner.StartTime) lcSep 
   fTutkaCTime(CallScanner.EndTime) lcSep 
   CallScanner.SearchRule lcSep 
   (IF CallScanner.AccessType EQ "" THEN "r" ELSE CallScanner.AccessType) SKIP.

   oiEvents = oiEvents + 1.
END.

OUTPUT STREAM Tutka CLOSE.

