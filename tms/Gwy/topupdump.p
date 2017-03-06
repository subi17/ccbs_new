/* -----------------------------------------------------------------
   MODULE .......: topupdump.p
   TASK .........: dump top-up information each day to file
   APPLICATION ..: TMS
   AUTHOR .......: as 
   CREATED ......: 07.09.07
   CHANGED ......: 
   Version ......: xfera 
   --------------------------------------------------- */               
                                                                
{Syst/commpaa.i} 
katun = "cron".
gcbrand = "1".

{Syst/eventlog.i}
{Func/date.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/xmlfunction.i}
{Func/tsformat.i}

DEF VAR lcSpooldir   AS C    NO-UNDO.
DEF VAR lcOutdir     AS C    NO-UNDO.
DEF VAR lcFilename   AS C    NO-UNDO.

DEF VAR lcNPOperType AS CHAR NO-UNDO.
DEF VAR lcIntOperID  AS CHAR NO-UNDO.
DEF VAR lcOperID     AS CHAR NO-UNDO.
DEF VAR lcOperType   AS CHAR NO-UNDO.

FUNCTION fFormatTS RETURNS CHAR
(its AS dec):
   def var ldadate as date no-undo.
   def var liTime as i no-undo.
   fSplitTS(its,ldaDate,liTime).
   RETURN
      STRING(YEAR(ldadate),"9999") +
      STRING(MONTH(ldadate),"99") +
      STRING(DAY(ldadate),"99") + " " +
      STRING(liTime,"hh:mm:ss").
END FUNCTION.

FUNCTION fConvert RETURNS CHAR
(icParam as char):
   case icParam:
      when "ANT" THEN RETURN "ANU".
      when "RCG" THEN RETURN "REC".
   end.
   return icParam.
END.

ASSIGN
   session:numeric-format = "AMERICAN"
   lcoutdir   =  fCParam("dumpoutgoing","topupdump.p") 
   lcspooldir =  fCParam("dumpspool","topupdump.p")
   lcfilename = "top_up_information_ccbs_" + STRING(fdatefmt(TODAY,"yyyymmdd"))
   + ".dat". 

OUTPUT STREAM excel TO VALUE(lcspooldir + lcfilename).
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

FOR EACH PrepaidRequest NO-LOCK WHERE
         PrepaidRequest.Brand = gcBrand AND
         PrepaidRequest.TSRequest > fMake2dt(TODAY - 1,1800) AND
         PrepaidRequest.TSRequest < fMake2dt(TODAY, 1799):
   
   IF Prepaidrequest.Source EQ "ATM" THEN DO:
      ASSIGN   
         lcNPOperType   = PrepaidRequest.Request
         lcIntOperID    = STRING(PrepaidRequest.PPRequest)
         lcOperID       = "900" + STRING(PrepaidRequest.PPRequest)
         lcOperType     = PrepaidRequest.Request.
   END.
   ELSE DO:   
      ASSIGN
         lcNPOperType   = ""
         lcIntOperID    = SUBSTRING(STRING(PrepaidRequest.PPRequest),3,
            LENGTH(STRING(PrepaidRequest.PPRequest)), "CHARACTER").
         lcOperID       = STRING(PrepaidRequest.PPRequest).
      CASE SUBSTRING(STRING(PrepaidRequest.PPRequest),1,2, "CHARACTER"):
         WHEN "97" THEN lcOperType = "INI".
         WHEN "99" THEN lcOperType = "MCA".
         WHEN "98" THEN lcOperType = "MCO".
         WHEN "96" THEN lcOperType = "MIN".
         WHEN "95" THEN lcOperType = "MAN".
      END.
   END.

   PUT STREAM excel UNFORMATTED
      PrepaidRequest.CLI   TAB      /* MSISDN */
      fConvert(lcNPOperType) TAB
      fGetNodeValue(PrepaidRequest.CommLine,"NumOper") TAB
      PrepaidRequest.TopUpAmt + PrepaidRequest.VatAmt TAB
      fTSFormat("yyyymmdd HH:MM:ss", Prepaidrequest.TSRequest) TAB 
      lcIntOperID TAB
      lcOperID TAB
      fConvert(lcOperType) TAB

      PrepaidRequest.Source TAB
      PrepaidRequest.Request TAB
      PrepaidRequest.TopUpAmt TAB
      PrepaidRequest.TaxZone TAB
      fGetRPCNodeValue(PrepaidRequest.Response,"responseCode") TAB
      
      PrepaidRequest.RespCode TAB
      fTSFormat("yyyymmdd HH:MM:ss", Prepaidrequest.TSResponse) SKIP. 

END.

OUTPUT STREAM excel CLOSE.

UNIX SILENT VALUE("mv " + lcSpoolDir + lcfilename + " " + lcoutdir).
