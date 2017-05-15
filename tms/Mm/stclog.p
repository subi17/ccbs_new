/* -----------------------------------------------------------------
   MODULE .......: stclog.p
   TASK .........: Dumps STC changes from TARJ(2) TO CONT(2)
   APPLICATION ..: TMS 
   AUTHOR .......: as 
   CREATED ......: 12/2007
   CHANGED ......: 
   Version ......: xfera 
   --------------------------------------------------- */               
                                                                
{Syst/commpaa.i} 
katun = "cron".
gcbrand = "1".

{Func/cparam2.i}
{Func/tsformat.i}

DEF VAR lcSpooldir   AS C    NO-UNDO.
DEF VAR lcOutdir     AS C    NO-UNDO.
DEF VAR lcFilename   AS C    NO-UNDO.
DEF VAR ldtStartD    AS DATE NO-UNDO.
DEF VAR ldtEndD      AS DATE NO-UNDO.
DEF VAR lcBalance    AS CHAR NO-UNDO.
DEF VAR ldIndexFind  AS DEC  NO-UNDO.

DEFINE STREAM excel.
DEFINE VARIABLE w AS INTEGER NO-UNDO.

DEF BUFFER bOldType FOR CLIType.

w = WEEKDAY(TODAY) - 1.
IF WEEKDAY(TODAY) = 1 THEN w = 7.

ASSIGN
   session:numeric-format = "AMERICAN"
   ldtStartD = TODAY - 6 - w 
   ldtEndD = TODAY - w 
   ldIndexFind = fMake2Dt(ldtStartD - 60,0)
   lcoutdir   =  fCParam("dumpoutgoing","stclog.p") 
   lcspooldir =  fCParam("dumpspool","stclog.p")
   /* This report will be named by the date interval of dump. */
   lcFileName = "stc_balances_" + 
   fDateFMT(ldtStartD, "YYYYMMDD") + "_" +
   fDateFMT(ldtEndD, "YYYYMMDD") + ".txt".

OUTPUT STREAM excel TO VALUE(lcspooldir + lcfilename).

FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand = gcBrand AND
         MsRequest.ReqType = 0 AND
         MsRequest.ReqStatus = 2 AND
         MsRequest.ActStamp >= ldIndexFind AND
         MsRequest.DoneStamp >= fMake2dt(ldtStartD,0) AND 
         MsRequest.DoneStamp < fMake2dt(ldtEndD + 1, 0):

   FIND FIRST bOldType WHERE
              bOldType.Brand = gcBrand AND
              bOldType.CLIType = MsRequest.ReqCParam1 NO-LOCK NO-ERROR.
   FIND FIRST CLIType WHERE
              CLIType.Brand = gcBrand AND
              CLIType.CLIType = MsRequest.ReqCParam2 NO-LOCK NO-ERROR.
              
   IF NOT AVAILABLE bOldType OR NOT AVAILABLE CLIType THEN NEXT. 
   
   IF bOldType.PayType NE 2 OR CLIType.PayType = 2 THEN NEXT.
   
   if index(MsRequest.Memo,"BALANCE") = 0 then do:
      lcBalance = "N/A".
   end.
   else do:
      lcBalance = SUBSTRING(MsRequest.Memo,INDEX(MsRequest.Memo,"BALANCE")).   
      lcBalance = ENTRY(2,ENTRY(1,lcBalance,","),"=").
   end.   

   PUT STREAM excel UNFORMATTED
      MsRequest.CLI        "|"
      MsRequest.MsSeq      "|"
      lcBalance "|"
      fTSFormat("yyyymmdd.HHMMss", MsRequest.DoneStamp) "|"
      MsRequest.ReqCParam1 "->" MsRequest.ReqCParam2
   SKIP.

END.

OUTPUT STREAM excel CLOSE.

UNIX SILENT VALUE("mv " + lcSpoolDir + lcfilename + " " + lcoutdir).
