/* ---------------------------------------------------------------------------
  MODULE .......: READPAYMB.P
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for reading payments from files            
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 13.05.05
  CHANGED ......: 03.05.07/aam posting date to file name
                  10.05.07/aam ActionLog,
                               Info from readpaym
                  16.05.07/aam empty posting file for control purposes if 
                               no files available 
  Version ......: M15
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "RetFile".
       
{Func/cparam2.i}
{Syst/utumaa.i "new"}
/* temp-table */
{Ar/paymfile.i}
{Ar/paymtrans.i}
{Func/farplog.i}
{Syst/eventlog.i}
{Func/timestamp.i}

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD PaymFile AS CHAR
   FIELD ConvMod  AS CHAR
   FIELD PaymCfg  AS CHAR
   FIELD AccNum   AS INT
   INDEX PaymFile PaymFile.

DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR lcPaymFile  AS CHAR NO-UNDO.
DEF VAR liFileType  AS INT  NO-UNDO. 
DEF VAR liRead      AS INT  NO-UNDO. 
DEF VAR liFiles     AS INT  NO-UNDO.
DEF VAR ldtFileDate AS DATE NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR lcInfo      AS CHAR NO-UNDO.

DEF STREAM sRead.

FUNCTION fCollTemp RETURNS LOGIC
   (icPaymFile AS CHAR).

   /* file not found */
   IF icPaymFile = "" OR SEARCH(icPaymFile) = ? THEN NEXT.
   
   IF CAN-FIND(FIRST ttFiles WHERE ttFiles.PaymFile = icPaymFile) THEN NEXT.
   
   CREATE ttFiles.
   ASSIGN ttFiles.PaymFile = icPaymFile
          ttFiles.ConvMod  = PaymCfg.ConvMod
          ttFiles.PaymCfg  = PaymCfg.PaymCfg
          ttFiles.AccNum   = PaymCfg.PaymAccNum.
   
END FUNCTION.


/* payment configuration not done */
IF NOT CAN-FIND(FIRST PaymCfg WHERE PaymCfg.Brand = gcBrand) THEN RETURN.

FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Company THEN ynimi = Company.CompName.

fELog("READPAYM","Started").

/* read all possible origins */
FOR EACH PaymCfg WHERE
         PaymCfg.Brand = gcBrand NO-LOCK:

   IF SEARCH(PaymCfg.ConvMod + ".r") = ? THEN NEXT. 
   
   /* have wild-cards been used */
   IF INDEX(PaymCfg.PaymFile,"*") GT 0 OR
      INDEX(PaymCfg.PaymFile,"?") GT 0
   THEN DO:
   
      RUN pFindFiles (PaymCfg.PaymFile,
                      OUTPUT lcPaymFile).
                      
      IF lcPaymFile > "" THEN 
      DO liCnt = 1 TO NUM-ENTRIES(lcPaymFile,"¤"):
         fCollTemp(ENTRY(liCnt,lcPaymFile,"¤")).
      END.
      
   END.                    

   /* single file */
   ELSE DO:
      fCollTemp(PaymCfg.PaymFile).  
   END.
       
END.

FOR EACH ttFiles:

   /* delete possible old ones */
   EMPTY TEMP-TABLE ttPayment.

   /* type of payment file */
   CASE ttFiles.ConvMod:
   WHEN "nnocko"     THEN liFileType = 1.
   WHEN "nnockott"   THEN liFileType = 2.
   WHEN "nnockointr" OR
   WHEN "nnockoakf"  THEN liFileType = 3.
   OTHERWISE liFileType = 0.
   END CASE. 
   
  
   liFiles = liFiles + 1.
   
   RUN VALUE(ttFiles.ConvMod) (INPUT  ttFiles.PaymFile, 
                               INPUT  ttFiles.AccNum, 
                               INPUT  ttFiles.PaymCfg,
                               OUTPUT TABLE ttPayment,
                               OUTPUT liCnt).

   /* no payments found from file -> move file anyway to processed */
   IF liCnt = 0 THEN DO:
      RUN pDeleteFile(ttFiles.PaymFile).
      NEXT. 
   END.

   FIND FIRST ttPayment NO-ERROR.
   IF AVAILABLE ttPayment 
   THEN ldtFileDate = ttPayment.AccDate.
   ELSE ldtFileDate = TODAY.
 
   /* booking list */
   oso  = "/tmp/" + ttFiles.PaymCfg + "_"     +
          STRING(YEAR(ldtFileDate),"9999") +
          STRING(MONTH(ldtFileDate),"99")  +
          STRING(DAY(ldtFileDate),"99")    +
          "_" + STRING(TIME) + ".txt".
     
   OUTPUT STREAM tul TO VALUE(oso).

   /* make sure that normal print routine is not used */
   str1 = "". 

   /* page length */
   ASSIGN spit1  = 80
          skayt1 = 80.

   RUN Ar/readpaym.p (INPUT TABLE ttPayment,
                 ttFiles.PaymFile,
                 liFileType,
                 FALSE,   /* show messages */
                 TRUE,    /* send mail     */
                 OUTPUT liRead,
                 OUTPUT lcInfo).

   /* update log */
   fCreateArplog(ttFiles.PaymFile,
                 ttFiles.PaymCfg,
                 katun). 

   DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "Cron"  
         ActionLog.KeyValue     = "" 
         ActionLog.ActionID     = "DDRetFile"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.ActionDec    = liRead
         ActionLog.ActionChar   = lcInfo
         ActionLog.ActionStatus = 3.
         ActionLog.ActionTS     = fMakeTS().

      /* file without the dir */
      lcPlainFile = ttFiles.PaymFile.
      IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
         lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").
         
      ActionLog.KeyValue = lcPlainFile.
   
   END.

   /* delete or move the payment file to archive */
   IF liRead > 0 THEN RUN pDeleteFile(ttFiles.PaymFile). 
   
END.

/* empty posting file for control purposes */
IF liFiles = 0 THEN DO:
   
   RUN Ar/readpaym.p (INPUT TABLE ttPayment,
                 "EMPTY",
                 0,
                 FALSE,    /* show messages */
                 FALSE,    /* send mail     */
                 OUTPUT liRead,
                 OUTPUT lcInfo).
END.

fELog("READPAYM","Stopped:" + STRING(liFiles)).


PROCEDURE pFindFiles:

   DEF INPUT  PARAMETER icFilter AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER ocFiles  AS CHAR NO-UNDO. 
       
   DEF VAR lcFileName AS CHAR NO-UNDO.
   
   INPUT STREAM sRead THROUGH VALUE("ls -1 " + icFilter).

   REPEAT:
      IMPORT STREAM sRead UNFORMATTED lcFileName.
      
      IF lcFileName = "" OR 
         lcFileName MATCHES ("*o such file or*") 
      THEN NEXT.
      
      ocFiles = ocFiles + 
                (IF ocFiles > "" THEN "¤" ELSE "") + 
                lcFileName.
   END.
                        
   INPUT STREAM sRead CLOSE.

END PROCEDURE.



