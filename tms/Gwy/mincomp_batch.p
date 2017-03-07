/* -----------------------------------------------
  MODULE .......: MINCOMP_BATCH.P
  FUNCTION .....: MinimumCompensation for PrePaids
  APPLICATION ..: TMS
  CREATED ......: 28.06.2007 KL
  MODIFIED .....: 05.07.2007 kl PrePaidRequest.UserCode
                  23.08.2007/aam prefix 996 -> 96
                  08.10.2007/aam MsSeq, PPReqPrefix,
                                 correct sign for vat amount
                  05.12.2007 kl liRespCode into procedure
                                update TSResponse field
                                liRespCode initally 9
                  10.01.08/aam  always vat0%
                  24.01.08/aam  payments through TopupQueue
  VERSION ......: XFERA
------------------------------------------------------ */

{Syst/commpaa.i}

ASSIGN
   katun   = "PPMINC"
   gcBrand = "1".

{Func/timestamp.i}
{Func/cparam2.i}
{Func/tmsparam4.i}
{Func/xmlfunction.i}
{Func/fgettxt.i}
{Func/ftaxdata.i}
{Func/tsformat.i}
{Func/ftransdir.i}
{Syst/tmsconst.i}

FUNCTION fCallAlarm RETURNS LOGICAL
  (INPUT pcAction AS CHARACTER,
   INPUT pcCLI    AS CHARACTER,
   INPUT pdeAmt   AS INTEGER):
   
   DEFINE VARIABLE ldeActStamp  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcAlarmMess  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLang       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcDate       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcTime       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaDate      AS DATE      NO-UNDO.
   DEFINE VARIABLE liTime       AS INTEGER   NO-UNDO.

   FIND FIRST MobSub WHERE
              MobSub.CLI = pcCLI
   NO-LOCK NO-ERROR.

   ASSIGN
      /* others don't exist yet ! */
      liLang      = 1
      lcAlarmMess = fGetSMSTxt(pcAction, TODAY, liLang, OUTPUT ldeActStamp)
      lcAlarmMess = REPLACE(lcAlarmMess,"#AMOUNT", 
                            TRIM(STRING(pdeAmt / 100,"->>>>9.99"))).
   
   IF lcAlarmMess > "" THEN DO:
      
      CREATE CallAlarm.
      ASSIGN
         CallAlarm.ActStamp   = ldeActStamp
         CallAlarm.CLSeq      = 0
         CallAlarm.CASeq      = NEXT-VALUE(CallAlarm)
         CallAlarm.CustNo     = MobSub.CustNum
         CallAlarm.CLI        = MobSub.CLI
         CallAlarm.DeliStat   = 1
         CallAlarm.Delitype   = 1
         CallAlarm.DeliPara   = "1"
         CallAlarm.DeliMsg    = lcAlarmMess
         CallAlarm.Limit      = 0
         CallAlarm.CreditType = 42
         CallAlarm.Orig       = "622"
         CallAlarm.ActInterval = "28800-75600" /* 8:00-21:00 */
         CallAlarm.Brand      = gcBrand.
         
      RELEASE CallAlarm.
   END.

END FUNCTION.

DEFINE TEMP-TABLE ttResponse NO-UNDO
   FIELD tcCLI AS CHARACTER
   FIELD tiAdj AS INTEGER
   FIELD tiAmt AS INTEGER
   FIELD tiErr AS INTEGER
   FIELD tcMsg AS CHARACTER.

DEFINE VARIABLE lcLine     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLI      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liMsSeq    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcErr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liAmt      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liCurrBal  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liAdjBal   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liErrAmt   AS INTEGER   NO-UNDO.
DEFINE VARIABLE llSMS      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcMsg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCLIPos   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liAmtPos   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liSMSPos   AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcMincFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRespFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcXML      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTaxZone  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcImpDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDoneDir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcErrorDir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeTS      AS DECIMAL   NO-UNDO.

ASSIGN
  lcImpDir   = fCParam("PREPAIDMC","mcincoming")
  lcDoneDir  = fCParam("PREPAIDMC","mcprocessed")
  lcOutDir   = fCParam("PREPAIDMC","mcoutgoing")
  lcSpoolDir = fCParam("PREPAIDMC","mcspool")
  lcErrorDir  = fCParam("PREPAIDMC","mcerror").
  
FUNCTION fPos RETURNS CHARACTER
  (INPUT piPos AS INTEGER):

   RETURN ENTRY(piPos,lcLine,"|").

END.

DEFINE VARIABLE ldThisRun  AS DECIMAL NO-UNDO. 
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liTotal    AS INTEGER NO-UNDO. 
DEFINE VARIABLE liAIROk    AS INTEGER NO-UNDO. 
DEFINE VARIABLE liAIRErr   AS INTEGER NO-UNDO. 
DEFINE VARIABLE liSMS      AS INTEGER NO-UNDO. 
DEFINE VARIABLE liNoBal    AS INTEGER NO-UNDO.
DEFINE VARIABLE liNotTarj  AS INTEGER NO-UNDO.

DEFINE STREAM sFile.
DEFINE STREAM sLine.
DEFINE STREAM sResponse.

INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcImpDir).

REPEAT:
   
   IMPORT STREAM sFile UNFORMATTED lcMincFile.
   
   lcFileName = lcMincFile.
   lcMincFile = lcImpDir + lcMincFile.
   
   IF SEARCH(lcMincFile) NE ? THEN DO:

      INPUT STREAM sLine FROM VALUE(lcMincFile).
      
      ASSIGN
         lcRespFile = REPLACE(lcMincFile,lcImpDir,lcSpoolDir)
         lcRespFile = REPLACE(lcRespFile,"TMSIN","TMSOUT").
    
      RUN pMarkStarted.
      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fTransDir(lcMincFile,
                "",
                lcErrorDir).
         LEAVE.
      END.
      ELSE IF RETURN-VALUE NE "OK" THEN
         LEAVE.
      
      OUTPUT STREAM sResponse TO VALUE(lcRespFile).
      
      EMPTY TEMP-TABLE ttResponse.

      liTotal = 0. liAIROk = 0. liAIRErr = 0. liSMS = 0. liNoBal = 0.
      liNotTarj = 0.

      REPEAT:
   
         IMPORT STREAM sLine UNFORMATTED lcLine.

         ASSIGN
            liCLIPos = 1
            liAmtPos = 2
            liSMSPos = 3
            lcCLI    = TRIM(fPos(liCLIPos))
            liAmt    = INT(ENTRY(1,fPos(liAmtPos)," "))
            llSMS    = (fPos(liSMSPos) = "Y")
            liTotal  = liTotal + 1.

         CREATE ttResponse.
         ASSIGN
            ttResponse.tcCLI = lcCLI
            ttResponse.tiAmt = liAmt
            ttResponse.tcMsg = "OK".

         FIND FIRST MobSub WHERE
                    MobSub.CLI = lcCLI
         NO-LOCK NO-ERROR.
 
         IF NOT AVAIL MobSub OR MobSub.PayType = FALSE THEN DO:
         ttResponse.tcMsg = "ERROR: No MSISDN in the system or NOT TARJ".
         liNotTarj = liNotTarj + 1. /* there is no TARJ or at all mobsub */
         END.
         ELSE DO:
            
            FIND FIRST Customer WHERE
                       Customer.CustNum = MobSub.CustNum
            NO-LOCK NO-ERROR.
   
            ASSIGN
               lcTaxZone  = fRegionTaxZone(Customer.Region)
               liMsSeq    = MobSub.MsSeq. 
 
            /* check current balance */
            liCurrBal = 0.
            RUN Gwy/balancequery.p(lcCLI).
            liCurrBal = INT(RETURN-VALUE) NO-ERROR.

            liAdjBal = liCurrBal - liAmt.

            IF   liAdjBal <= 0 THEN liAdjBal = liCurrBal.
            ELSE liAdjBal  = liAmt.
            
            liErrAmt = liAmt - liAdjBal.

            /* OK to adjust balance */
            IF liAdjBal > 0 THEN RUN pAdjustBalance.
            /* nothing could be done */
            ELSE DO:
               ldeTS = fMakeTS().
               liNoBal = liNoBal + 1.

               CREATE TopUpQueue.
               ASSIGN
                  TopUpQueue.PPRequest = ?
                  TopUpQueue.CLI       = lcCLI
                  TopUpQueue.TopUpAmt  = 0 - (liErrAmt / 100)
                  TopUpQueue.VatAmt    = 0    /* always without tax */
                  TopUpQueue.Date      = TODAY
                  TopUpQueue.Source    = "MCNOC".

               ASSIGN
                  ttResponse.tiAdj = 0
                  ttResponse.tiErr = liErrAmt + liAdjBal
                  ttResponse.tcMsg = "ERROR: Balance is " + STRING(liCurrBal).

            END.
      
         END.

         PUT STREAM sResponse UNFORMATTED
            ttResponse.tcCLI         "|"
            STRING(ttResponse.tiAdj) "|"
            STRING(ttResponse.tiAmt) "|"
            STRING(ttResponse.tiErr) "|"
            ttResponse.tcMsg         "|"
            fTSFormat("ddmmyyyyHHMMss", ldeTS) CHR(10).

      END.
   
      OUTPUT STREAM sResponse CLOSE.
      fTransDir(lcRespFile,
                "",
                lcOutDir).

      fTransDir(lcMincFile,
                "",
                lcDoneDir).
      RUN pMarkFinished.
   END.

END.

PROCEDURE pAdjustBalance:
   
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
      PrePaidRequest.MsSeq       = liMsSeq
      PrePaidRequest.CLI         = lcCLI
      PrePaidRequest.PPRequest   = liRequest
      PrePaidRequest.PPReqPrefix = "996"
      PrePaidRequest.Request     = "AdjustmentTRequest"
      PrePaidRequest.CommLine    = "AdjustmentTRequest"
      PrePaidRequest.Source      = "MINCONS"
      PrePaidRequest.PPStatus    = 0
      PrePaidRequest.TopUpAmt    = 0 - liAdjBal
      /* minimum consumption is always without tax */
      PrePaidRequest.VatAmt      = 0
      PrePaidRequest.TaxZone     = lcTaxZone.
   
   RUN Gwy/pp_platform.p(gcBrand,PrePaidRequest.PPRequest).
   
   lcXML = RETURN-VALUE.
   ldeTS = fMakeTS().
   
   IF lcXML > "" AND NOT lcXML BEGINS "ERR:" THEN
      liRespCode = INT(fGetRPCNodeValue(lcXML,"responseCode")) NO-ERROR.

   ASSIGN
      PrePaidRequest.Response   = lcXML
      PrePaidRequest.RespCode   = liRespCode
      PrePaidRequest.TSResponse = ldeTS.

   /* OK response */
   IF liRespCode <= 2 THEN DO:
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

      /* payment for not adjusted */
      IF liErrAmt > 0 THEN DO:
         CREATE TopUpQueue.
         ASSIGN
            TopUpQueue.PPRequest = PrePaidRequest.PPRequest
            TopUpQueue.CLI       = PrePaidRequest.CLI
            TopUpQueue.TopUpAmt  = 0 - (liErrAmt / 100)
            TopUpQueue.VatAmt    = 0   /* always without tax */
            TopUpQueue.Date      = TODAY
            TopUpQueue.Source    = "MCNOC".
      END.
         
      CREATE Memo.
      ASSIGN
         Memo.CreStamp  = fMakeTS()
         Memo.Brand     = gcBrand
         Memo.HostTable = "MobSub"
         Memo.KeyValue  = STRING(MobSub.MsSeq)
         Memo.CustNum   = MobSub.CustNum
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = katun
         Memo.MemoTitle = "Minimum Consumption"
         Memo.MemoText  = "Subscription's balance has been charged with " +
            TRIM(STRING(ABS(ROUND(PrePaidRequest.TopUpAmt / 100,2)),
                        "->>>>>>>>>9.99")) +
            " euros Minus adjustment as minimum consumption.".
      
      ASSIGN
         ttResponse.tiAdj = liAdjBal
         ttResponse.tiErr = liErrAmt.

      /* SMS when requested */
      IF llSMS THEN DO:
         fCallAlarm("PPMinimCons",
                    PrePaidRequest.CLI,
                    ABS(INT(liAdjBal))).
         liSMS = liSMS + 1.
      END.

      liAIROk = liAIROk + 1.
   END.
   /* error occured */
   ELSE DO:
      PrePaidRequest.PPStatus = 2.
      ASSIGN
         ttResponse.tiAdj = 0
         ttResponse.tiErr = liErrAmt + liAdjBal
         ttResponse.tcMsg = "AIR ERROR: " + STRING(PrePaidRequest.RespCode).
      liAIRErr = liAIRErr + 1.
   END.
 
END.

PROCEDURE pMarkStarted:
   
   DEFINE VARIABLE lcError AS CHAR NO-UNDO. 

   DEF VAR lcExpectedFileName AS CHAR NO-UNDO. 
   DEF VAR ldaPrevMonth AS DATE NO-UNDO. 
   DEF VAR liLogStatus AS INT NO-UNDO. 
   DEF VAR llRerunAllowed AS LOG NO-UNDO. 
   
   llRerunAllowed = (fCParamI("MinConsRerunAllowed") eq 1).
      
   ASSIGN    
      ldThisRun  = fMakeTS()
      ldaPrevMonth = ADD-INTERVAL(TODAY, -1, "months").
      lcExpectedFileName = "yoigo_MCP_TMSIN_" +
         STRING(MONTH(ldaPrevMonth),"99") + "-" +
         STRING(YEAR(ldaPrevMonth)) + "_".

   IF fCheckFileNameChars(lcFileName) EQ FALSE THEN DO:
      ASSIGN lcError = "ERROR:Incorrect file name syntax"
             liLogStatus = 1.
   END.
   ELSE IF NOT lcFileName BEGINS lcExpectedFileName THEN DO:
      ASSIGN lcError = "ERROR:Incorrect file name month or year"
             liLogStatus = 1.
   END.
   /* check that there isn't already another run for the same purpose */
   ELSE IF CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
                     ActionLog.Brand        = gcBrand     AND    
                     ActionLog.ActionID     = "MINCONS" AND                     
                     ActionLog.ActionPeriod > 201201 AND
                     ActionLog.ActionStatus = 0 AND
                     actionlog.actionts > 20120101) THEN DO:
      /* this file is already running */
      FIND FIRST ActionLog USE-INDEX ActionID WHERE
                 ActionLog.Brand        = gcBrand     AND
                 ActionLog.ActionID     = "MINCONS" AND
                 ActionLog.ActionPeriod > 201201 AND
                 ActionLog.ActionStatus = 0 AND 
                 Actionlog.actionts > 20120101             
                 NO-LOCK NO-ERROR.      

      IF (ActionLog.KeyValue EQ lcFileName) THEN RETURN "SKIPPED".

      /* new file tryed to run during ongoing run */
      ASSIGN lcError = "ERROR:Batch not started due to ongoing run"
             liLogStatus = 3.
   END.
   ELSE IF CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
                     ActionLog.Brand        = gcBrand     AND    
                     ActionLog.ActionID     = "MINCONS" AND
                     ActionLog.KeyValue     = lcFileName AND
                     ActionLog.ActionStatus = 2 AND
                     Actionlog.actionts > 20120101) THEN DO: 
      ASSIGN lcError = "ERROR:File already handled"
             liLogStatus = 1.
   END.
   /* check if file is already handled during this month */
   ELSE IF CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
               ActionLog.Brand        = gcBrand     AND
               ActionLog.ActionID     = "MINCONS" AND
               ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY) AND
               Actionlog.actionts > 20120101) AND
               llRerunAllowed EQ FALSE THEN DO:
         ASSIGN lcError = "ERROR:Batch not started due to existing run on this month"
                liLogStatus = 1.
   END.
   ELSE IF CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
               ActionLog.Brand        = gcBrand     AND
               ActionLog.ActionID     = "MINCONS" AND
               ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY) AND
               Actionlog.actionts > 20120101) AND 
               llRerunAllowed EQ TRUE THEN
      fCParam4SetI("1","PREPAIDMC","MinConsRerunAllowed", 0).
      /*Allow only once after flag is switched on */

   IF lcError > "" THEN DO:
      
      DO TRANS:
         CREATE ActionLog.
         
         ASSIGN
            ActionLog.Brand        = gcBrand
            ActionLog.ActionID     = "MINCONS"
            ActionLog.ActionTS     = ldThisRun
            ActionLog.TableName    = "Cron"
            ActionLog.KeyValue     = lcFileName
            ActionLog.UserCode     = katun
            ActionLog.ActionStatus = liLogStatus
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY) 
            ActionLog.ActionChar   = lcError.
         RELEASE ActionLog.   
      END.
      RETURN lcError. 
   END.

   /* mark this run started */
   DO TRANS:
      CREATE ActionLog.
      
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.ActionID     = "MINCONS"
         ActionLog.ActionTS     = ldThisRun
         ActionLog.TableName    = "Cron"
         ActionLog.KeyValue     = lcFileName
         ActionLog.UserCode     = katun
         ActionLog.ActionStatus = 0
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).
      RELEASE ActionLog.   
   END.
   RETURN "OK".

END PROCEDURE.
   
PROCEDURE pMarkFinished:

   /* mark this run finished */
   FOR FIRST ActionLog USE-INDEX ActionID WHERE
             ActionLog.Brand        = gcBrand AND    
             ActionLog.ActionID     = "MINCONS" AND
             ActionLog.ActionTS     = ldThisRun AND
             ActionLog.TableName    = "Cron" AND
             ActionLog.ActionStatus = 0 AND
             ActionLog.KeyValue     = lcFileName 
   EXCLUSIVE-LOCK:
      ASSIGN 
        ActionLog.ActionStatus = 2
        ActionLog.ActionDec = fMakeTS().
        ActionLog.ActionChar   = SUBST("TOTAL: &1, AIR OK: &2, AIR ERROR: &3, SMS SENT: &4, NO BAL: &5 NOT TARJ: &6 ", liTotal, liAIROk, liAIRErr, liSMS, liNoBal, liNotTarj).
   END.
   
END PROCEDURE.
