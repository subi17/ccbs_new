{commpaa.i}

ASSIGN
   katun   = "cron"
   gcBrand = "1".

{timestamp.i}
{cparam.i2}
{xmlfunction.i}
{fgettxt.i}
{ftaxdata.i}
{tsformat.i}

DEFINE VARIABLE lcXML      AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeTaxPerc AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liVoucher  AS INTEGER   NO-UNDO.

DEF VAR lcFile       AS CHAR NO-UNDO.
DEF VAR lcLogFileOk  AS CHAR NO-UNDO.
DEF VAR lcLogFileNok AS CHAR NO-UNDO.

DEF VAR lcLine       AS CHAR NO-UNDO.
DEF VAR liMsSeq      AS INT  NO-UNDO.
DEF VAR lcCLI        AS CHAR NO-UNDO.
DEF VAR lcICC        AS CHAR NO-UNDO.
DEF VAR lcSep        AS CHAR NO-UNDO.
DEF VAR ldCurrent    AS DEC  NO-UNDO.
DEF VAR lcSource     AS CHAR NO-UNDO.
DEF VAR lcError      AS CHAR NO-UNDO.
DEF VAR lcPlainFile  AS CHAR NO-UNDO.
DEF VAR liCnt        AS INT  NO-UNDO.
DEF VAR liRequest    AS INT  NO-UNDO.
DEF VAR ldBalStamp   AS DEC  NO-UNDO.
DEF VAR ldAmount     AS DEC  NO-UNDO.
DEF VAR ldCurrBal    AS DEC  NO-UNDO.
DEF VAR lcTaxZone    AS CHAR NO-UNDO.
DEF VAR liRead       AS INT  NO-UNDO. 
DEF VAR liErrors     AS INT  NO-UNDO.
DEF VAR liOk         AS INT  NO-UNDO.
DEF VAR ldVatPerc    AS DEC  NO-UNDO.
DEF VAR liOrigReq    AS INT  NO-UNDO.

DEF STREAM sRead.
DEF STREAM sLogOk.
DEF STREAM sLogNok.


FUNCTION fError RETURNS LOGIC
   (iiError   AS INT,
    icMessage AS CHAR):
   
   PUT STREAM sLogNOk UNFORMATTED
      lcLine    lcSep
      iiError   lcSep
      icMessage SKIP.
   
   liErrors = liErrors + 1.
     
END FUNCTION.

FUNCTION fOk RETURNS LOGIC:
   
   PUT STREAM sLogOk UNFORMATTED
      lcLine SKIP.
   
   liOk = liOk + 1.
     
END FUNCTION.


lcFile       = "/apps/snet/200804/preactive_minusadj_080409.txt".
lcLogFileOk  = "/apps/snet/200804/minus_adjust_ok.log".
lcLogFileNOk = "/apps/snet/200804/minus_adjust_nok.log".

ASSIGN
   liCnt       = R-INDEX(lcFile,"/")
   lcPlainFile = lcFile
   lcSep       = CHR(9).
   
IF liCnt > 1 THEN 
   lcPlainFile = SUBSTRING(lcPlainFile,liCnt + 1).

INPUT STREAM sRead FROM VALUE(lcFile).
OUTPUT STREAM sLogOk TO VALUE(lcLogFileOk) APPEND.
OUTPUT STREAM sLogNOk TO VALUE(lcLogFileNOk) APPEND.


REPEAT:

   IMPORT STREAM sRead UNFORMATTED lcLine.
   
   liRead  = liRead + 1.
   
   ASSIGN 
      lcICC      = ENTRY(1,lcLine,lcSep)
      lcCLI      = ENTRY(2,lcLine,lcSep)
      ldAmount   = DECIMAL(ENTRY(3,lcLine,lcSep))
      NO-ERROR.
      
   IF ERROR-STATUS:ERROR THEN DO:
      IF lcCLI = "MSISDN" THEN NEXT.
      
      fError(5,"").
      NEXT.
   END.
  
   FIND FIRST MobSub WHERE MobSub.CLI = lcCLI NO-LOCK NO-ERROR.

   IF NOT AVAILABLE MobSub OR MobSub.CLIType NE "Tarj3" THEN DO:
      fError(3,"").
      NEXT.
   END.
   
   IF lcICC NE MobSub.ICC THEN DO:
      fError(4,"").
      NEXT.
   END.

   /* a little shortcut, events should all be alike */
   IF MobSub.InvCust = 233718 AND ldAmount = 10 THEN ASSIGN
      lcTaxZone = "1"
      ldAmount  = 8.62.

   ELSE DO:
      /* take vat out from amount */
      FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK.
      ASSIGN 
         lcTaxZone = fRegionTaxZone(Customer.Region)
         ldVatPerc = fTaxPerc(lcTaxZone,"1").
      
      ldAmount = ROUND(ldAmount / (1 + ldVatPerc / 100),2).
   END.
   
   RUN balancequery(lcCLI).
   ldCurrBal = INT(RETURN-VALUE) / 100.

   IF ldCurrBal NE ldAmount THEN DO:
      fError(1,STRING(ldAmount) + "/" + STRING(ldCurrBal)).
      NEXT.
   END.

   liOrigReq = 0.
   for first prepaidrequest no-lock use-index msseq where
             prepaidrequest.brand = "1" and
             prepaidrequest.msseq = mobsub.msseq and
             prepaidrequest.ppreqpref = "992" and
             prepaidrequest.source = "web order":
      liOrigReq = prepaidrequest.pprequest.
   end.
   
   RUN pAdjustBalance (ldAmount).

   
   IF liRead MOD 10 = 0 THEN DO:
      PAUSE 0.
      DISP liRead liErrors liOk WITH 1 DOWN.
   END.
END.

INPUT STREAM sRead CLOSE.
OUTPUT STREAM sLogOk CLOSE.
OUTPUT STREAM sLogNOk CLOSE.

DISP liRead liErrors liOk.


PROCEDURE pAdjustBalance:
   
   DEF INPUT PARAMETER idAmount AS DEC NO-UNDO.
    
   DEFINE VARIABLE liRequest  AS INTEGER NO-UNDO.
   DEFINE VARIABLE liRespCode AS INTEGER NO-UNDO INIT 9.
   
   DO WHILE TRUE:
      liRequest = NEXT-VALUE(PrePaidReq).
   
      IF NOT CAN-FIND(FIRST PrePaidRequest WHERE
                            PrePaidRequest.Brand     = gcBrand AND
                            PrepaidRequest.PPRequest = liRequest)
      THEN LEAVE.
   END.
    
   CREATE PrePaidRequest.
   ASSIGN
      PrePaidRequest.TSRequest   = fMakeTS()
      PrePaidRequest.UserCode    = katun
      PrePaidRequest.Brand       = gcBrand
      PrePaidRequest.MsSeq       = MobSub.MsSeq
      PrePaidRequest.CLI         = MobSub.CLI
      PrePaidRequest.PPRequest   = liRequest
      PrePaidRequest.PPReqPrefix = "992"
      PrePaidRequest.Request     = "AdjustmentTRequest"
      PrePaidRequest.CommLine    = "AdjustmentTRequest"
      PrePaidRequest.Source      = "MANFIX"
      PrePaidRequest.PPStatus    = 99
      PrePaidRequest.TopUpAmt    = -1 * idAmount * 100
      PrePaidRequest.VatAmt      = 0
      PrePaidRequest.TaxZone     = lcTaxZone
      PrePaidRequest.OrigRequest = liOrigReq.
   
   RUN pp_platform(gcBrand,PrePaidRequest.PPRequest).
   
   lcXML = RETURN-VALUE.
   
   IF NOT lcXML BEGINS "ERR:" THEN
      liRespCode = INT(fGetRPCNodeValue(lcXML,"responseCode")) NO-ERROR.

   ASSIGN
      PrePaidRequest.Response   = lcXML
      PrePaidRequest.RespCode   = liRespCode
      PrePaidRequest.TSResponse = fMakeTS().

   /* OK response */
   IF liRespCode = 0 THEN DO:
   
      PrePaidRequest.PPStatus = 2.
 
      /* payment for adjustment */
      CREATE TopUpQueue.
      ASSIGN
         TopUpQueue.PPRequest = PrePaidRequest.PPRequest
         TopUpQueue.CLI       = PrePaidRequest.CLI
         TopUpQueue.TopUpAmt  = PrePaidRequest.TopUpAmt / 100
         TopUpQueue.VatAmt    = PrePaidRequest.VatAmt / 100
         TopUpQueue.Date      = TODAY
         TopUpQueue.Source    = PrePaidRequest.Source.

      CREATE Memo.
      ASSIGN 
         Memo.Brand     = gcBrand
         Memo.HostTable = "MobSub"
         Memo.KeyValue  = STRING(MobSub.MsSeq)
         Memo.CustNum   = MobSub.CustNum
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = katun 
         Memo.MemoTitle = "MINUS ADJUSTMENT"
         Memo.MemoText  = "Deducted " + STRING(-1 * ldAmount) + 
                          " eur according to file " + lcPlainFile +
                          " (YCM-497).".
         Memo.CreStamp  = fMakeTS().
  
      fOK().
    
   END.

   /* error occured */
   ELSE DO:
      PrePaidRequest.PPStatus = 3.
      fError(2,"").
   END.
 
END.

