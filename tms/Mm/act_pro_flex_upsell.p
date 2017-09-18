/*--------------------------------------------------------------------
  MODULE .......: pro_flex_upsell_batch.p
  TASK .........: The program activates required FLEX_N_UPSELL or
                  DSS_FLEX_N_UPSELL to Pro subscriptions.
  APPLICATION ..: tms
  AUTHOR .......: Ilkka Savolainen
  CREATED ......: 25.7.2017
  Version ......: yoigo
---------------------------------------------------------------------- */
/*YPRO-88*/
{Syst/tmsconst.i}
{Syst/commpaa.i}
{Func/timestamp.i}
{Func/upsellbundle.i}
{Func/matrix.i}

gcBrand =  Syst.Parameters:gcBrand.
/*
"FLEX_500MB_UPSELL"
"FLEX_5GB_UPSELL"
"DSS_FLEX_500MB_UPSELL"
"DSS_FLEX_5GB_UPSELL"
*/

DEF STREAM sLogFile.
DEF VAR lcTmp AS CHAR NO-UNDO.
DEF VAR liCount AS INT NO-UNDO.
DEF VAR lcLogFile AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcUpsellList AS CHAR NO-UNDO.
DEF VAR lcLogRow AS CHAR NO-UNDO.
DEF VAR llgSimulate AS LOG NO-UNDO.
DEF VAR lcUpsell AS CHAR NO-UNDO.
DEF VAR lcSeekUpsell AS CHAR NO-UNDO.
DEF VAR lcError AS CHAR NO-UNDO.
DEF VAR liRequest AS INT NO-UNDO.
DEF VAR ldeNow AS DECIMAL.
DEF VAR ldeActTime AS DECIMAL.
DEF VAR ldeCheckMoment AS DECIMAL.
DEF VAR ldaReadDate  AS DATE NO-UNDO.

llgSimulate = TRUE. /*TRUE-> only log writing, FALSE->make real updates*/

FUNCTION fPrintClitype RETURNS CHAR
   (iiMsSeq AS INT):
   FIND FIRST mobsub NO-LOCK where
              mobsub.msseq eq iiMsSeq NO-ERROR.
   IF AVAIL mobsub THEN
      RETURN mobsub.clitype.
   RETURN "".   
              
END.   

FUNCTION fMonthStart RETURNS DECIMAL:
   DEF VAR ldeCurr AS DECIMAL NO-UNDO.
   ldeCurr =  INT( fMakeTS() / 100) * 100  .
   RETURN ldeCurr + 1.

END.
FUNCTION fPrevMonthEnd RETURNS DECIMAL:
   DEF VAR ldeCurr AS DECIMAL NO-UNDO.
   ldeCurr = (INT( fMakeTS() / 100) * 100) + 1.
   RETURN fSecOffSet(ldeCurr,-1). /*mont change - 2 secs*/
END.


ldeNow = fMakeTS().
ldeActTime = fMonthStart().
ldeCheckMoment = fPrevMonthEnd().
lcLogDir = fCParam("UpsellCron","UpsellLog").

IF lcLogDir EQ "" OR lcLogDir EQ ? THEN lcLogDir = "/tmp/".
ldaReadDate  = TODAY.
lcLogFile    = lcLogDir + "UpsellCronLog_" +
                      STRING(YEAR(ldaReadDate)) +
                      STRING(MONTH(ldaReadDate),"99") +
                      STRING(DAY(ldaReadDate),"99") +
                      REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log".


/*Harad coding for testing*/
ldeCheckMoment = ldeNow.

lcUpsellList = "FLEX_500MB_UPSELL,FLEX_5GB_UPSELL,DSS_FLEX_500MB_UPSELL,DSS_FLEX_5GB_UPSELL".


OUTPUT STREAM sLogFile TO VALUE(lcLogFile) APPEND.

DO liCount = 1 TO NUM-ENTRIES(lcUpsellList):
   lcSeekUpsell = ENTRY(liCount,lcUpsellList).
   FOR EACH servicelimit NO-LOCK where
            servicelimit.groupcode eq lcSeekUpsell:

      FOR EACH mservicelimit NO-LOCK WHERE
               mservicelimit.slseq EQ servicelimit.slseq AND
               mservicelimit.dialtype EQ servicelimit.dialtype AND
/*               mservicelimit.EndTS EQ ldeCheckMoment: /*previous month end*/ */
/*test condition:*/
               mservicelimit.EndTS > ldeNow AND
               (mservicelimit.msseq EQ 70088777 /*OR
               mservicelimit.msseq EQ 70088804 */ ):

/*end of test*/               
         /*Decide upsell that must be activated.
           If DSS is active -> activate DSS_FLEX_ 
           Else -> activate FLEX_.
         */
         message fGetActiveDSSId(mservicelimit.custnum, ldenow) VIEW-AS ALERT-BOX.
         IF fIsDSSActive(mservicelimit.custnum, 
                         ldeNow) 
         THEN DO:
            /*Select related DSS upsell*/
            IF servicelimit.groupcode MATCHES "*FLEX_500MB_UPSELL" THEN
               lcUpsell = "DSS_FLEX_500MB_UPSELL".
            ELSE IF servicelimit.groupcode MATCHES "*FLEX_5GB_UPSELL" THEN
               lcUpsell = "DSS_FLEX_5GB_UPSELL".
         END.   
         ELSE DO:
            /*Select related normal upsell*/
            IF servicelimit.groupcode MATCHES "*FLEX_500MB_UPSELL" THEN
               lcUpsell = "FLEX_500MB_UPSELL".
            ELSE IF servicelimit.groupcode MATCHES "*FLEX_5GB_UPSELL" THEN
               lcUpsell = "FLEX_5GB_UPSELL".
    
         END.
    
         IF llgSimulate EQ FALSE THEN DO:
            lcLogRow = "".
              
            lcUpsell = servicelimit.groupcode.
            IF fMatrixAnalyse(gcBrand,
                           "PERCONTR",
                           "PerContract;SubsTypeTo",
                           lcUpsell + ";" + fPrintCLIType(mservicelimit.msseq),
                           OUTPUT lcTmp) EQ 1 THEN DO:
                              
               fCreateUpsellBundle(mservicelimit.MsSeq,
                                   lcUpsell,
                                   {&REQUEST_SOURCE_YOIGO_TOOL},
                                   ldeActTime,
                                   OUTPUT liRequest,
                                   OUTPUT lcError).
                                
               IF lcError EQ "" THEN lcError = "OK".
             END.  
             ELSE DO:
               lcError = "Not allowed Upsell". 
             END.
             lcLogRow = STRING(mservicelimit.MsSeq) + ";" +
                        servicelimit.groupcode + ";" + /*prev month upsell */
                        lcUpsell + ";" + /*activated upsell */
                        lcError.

         END. /*Simulate == false */
         ELSE DO:
            lcLogRow = "".

            lcLogRow = STRING(mservicelimit.MsSeq) + ";" +
                       servicelimit.groupcode + ";" + /*previous month upsell */
                       lcUpsell + ";" + /*activatd upsell */
                       "Simulate". 
         END.

         
         PUT STREAM sLogFile UNFORMATTED lcLogRow SKIP.

      END.
   END.         
END.    
OUTPUT STREAM sLogFile CLOSE.

