 /* ------------------------------------------------------
  MODULE .......: invjournalb.p
  FUNCTION .....: invoice journal as batch
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 26.06.07 
  MODIFIED .....: 
  VERSION ......: Yoigo
  ------------------------------------------------------ */


{Syst/commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "laskutus".

{Func/date.i}
{Syst/utumaa.i "new"}
{Ar/invjournal.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Syst/eventlog.i}

DEF VAR lcFile     AS CHAR NO-UNDO.
DEF VAR lcBaseFile AS CHAR NO-UNDO.
DEF VAR lcTransDir AS CHAR NO-UNDO.
DEF VAR lcSpoolDir AS CHAR NO-UNDO. 
DEF VAR liPrint    AS INT  NO-UNDO.
DEF VAR ldtInvDate AS DATE NO-UNDO EXTENT 2. 
DEF VAR lcMonths   AS CHAR NO-UNDO. 
DEF VAR liInvQty   AS INT  NO-UNDO.
DEF VAR lcParam    AS CHAR NO-UNDO.
DEF VAR ok         AS LOG  NO-UNDO.
DEF VAR ldTmpDate  AS DATE NO-UNDO.

ASSIGN 
   lcBaseFile = fCParamC("InvJourFile")
   lcTransDir = fCParamC("InvJourTransDir")
   lcSpoolDir = fCParamC("InvJourSpoolDir")
   lcMonths   = "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec"
   /* page length */
   spit1      = 80
   skayt1     = 80.
   
IF lcFile     = ? THEN QUIT.
IF lcTransDir = ? THEN lcTransDir = "".
IF lcSpoolDir = ? OR lcSpoolDir = "" THEN lcSpoolDir = "/tmp".

FIND FIRST Company NO-LOCK.
ynimi = Company.CompName.

/* Parameter handling */

lcParam = SESSION:PARAMETER.

IF lcParam NE "" THEN DO:

   ok = TRUE.

   /* format dd/mm/yyyy-dd/mm/yyyy */
   IF INDEX(lcParam,"-") > 0 THEN DO:

      ldtInvDate[1] = DATE(TRIM(ENTRY(1,lcParam,"-"))) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN ok = false.
      ldtInvDate[2] = DATE(TRIM(ENTRY(2,lcParam,"-"))) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN ok = false.

   END.

   /* parse paramter format yyyymm */
   ELSE IF LENGTH(lcParam) EQ 6 THEN DO:

      ldtInvDate[1] =
         DATE(INT(SUBSTRING(lcParam,5,2)),
         1,
         INT(SUBSTRING(lcParam,1,4))) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN ok = false.
      ldtInvDate[2] = fLastDayOfMonth(ldtInvDate[1]).

   END.

   /* parse parameter format dd/mm/yyyy */
   ELSE DO:

      ldtInvDate[1] = DATE(TRIM(lcParam)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN ok = false.
      ldtInvDate[2] = ldtInvDate[1].

   END.

   IF NOT ok THEN DO:

      MESSAGE "Incorrect parameter: " + lcParam.
      PAUSE 0.
      RETURN.

   END.

END.
/* Handling for 1-15 and 16-Last day of month */

ELSE DO:
   
   /* Run dump from begining of current month to 15th */
   IF DAY(TODAY) >= 16 THEN DO:
      ldtInvDate[1] = DATE(MONTH(TODAY),1,YEAR(TODAY)).
      ldtInvDate[2] = DATE(MONTH(TODAY),15,YEAR(TODAY)).
   END.
   /* Run dump from halfway of previous month to end of previous month */
   ELSE DO:
      ldTmpDate = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
      ldtInvDate[1] = DATE(MONTH(ldTmpDate),16,YEAR(ldTmpDate)).
      ldtInvDate[2] = fLastDayOfMonth(ldTmpDate). 
   END.
END.



fELog("INVJOURNAL","Started").

/* print a detailed report for each invgroup and then a summary from all */
FOR EACH InvGroup NO-LOCK WHERE
         InvGroup.Brand = gcBrand:

   RUN pPrintReport(InvGroup.InvGroup,
                    ldtInvDate[1],
                    ldtInvDate[2],
                    TRUE,
                    TRUE).
END.
 
/* summary */
RUN pPrintReport("",
                 ldtInvDate[1],
                 ldtInvDate[2],
                 FALSE,
                 TRUE).
 
fELog("INVJOURNAL","Stopped").

PROCEDURE pPrintReport:

   DEF INPUT PARAMETER icInvGroup AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idtDate1   AS DATE NO-UNDO.
   DEF INPUT PARAMETER idtDate2   AS DATE NO-UNDO.
   DEF INPUT PARAMETER ilInvoices AS LOG  NO-UNDO.
   DEF INPUT PARAMETER ilSummary  AS LOG  NO-UNDO.

   DEF VAR lcPlainFile AS CHAR NO-UNDO.
   
   ASSIGN 
      lcFile = lcSpoolDir + "/" + lcBaseFile
      lcFile = REPLACE(lcFile,"#INVGRP",IF icInvGroup > ""
                                        THEN icInvGroup
                                        ELSE "ALL")
      lcFile = REPLACE(lcFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                      STRING(MONTH(TODAY),"99")  +
                                      STRING(DAY(TODAY),"99"))
      lcFile = REPLACE(lcFile,"#MONTH",ENTRY(MONTH(TODAY),lcMonths) +
                                       STRING(YEAR(TODAY))). 

   EMPTY TEMP-TABLE ttCriter.
   
   CREATE ttCriter.
   ASSIGN 
      ttCriter.InvGroup    = icInvGroup
      ttCriter.CustNum1    = 0
      ttCriter.CustNum2    = 999999999
      ttCriter.InvDate1    = idtDate1
      ttCriter.InvDate2    = idtDate2
      ttCriter.ExtInvID1   = ""
      ttCriter.ExtInvID2   = "ZZZZ"
      ttCriter.InvType1    = 0
      ttCriter.InvType2    = 98
      ttCriter.PaymState1  = 0
      ttCriter.PaymState2  = 9
      ttCriter.DenyRemind  = FALSE
      ttCriter.DenyPrint   = FALSE
      ttCriter.OnlyUnpaid  = FALSE
      ttCriter.ZeroVat     = FALSE
      ttCriter.Invoices    = ilInvoices
      ttCriter.InvAccounts = ilInvoices
      ttCriter.Summary     = ilSummary
      ttCriter.ToFile      = lcFile.

   RUN Ar/invjournal.p (INPUT TABLE TCustGroup,
                   INPUT TABLE ttCriter,
                   OUTPUT liInvQty).

   /* move the file to the transfer directory */
   IF lcTransDir > "" THEN DO:
      fTransDir(lcFile,
                ".txt",
                lcTransDir).
   END.


   /* save results to db for reporting */
   IF liInvQty = 0 OR RETURN-VALUE BEGINS "ERROR:" THEN DO TRANS:

      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "InvJournal"
             ErrorLog.TableName = "cron"
             ErrorLog.KeyValue  = ""
             ErrorLog.ErrorMsg  = STRING(liInvQty) + 
                                  " invoices were printed  " +
                                  RETURN-VALUE + CHR(10) +
                                  "InvGroup: " + 
                                  (IF icInvGroup > ""
                                   THEN icInvGroup
                                   ELSE "ALL") + CHR(10) + 
                                  "Period: " +
                                  STRING(idtDate1,"99.99.9999") + "-" +
                                  STRING(idtDate2,"99.99.9999")
             ErrorLog.UserCode  = katun.
             ErrorLog.ActionTS  = fMakeTS().
   END.
   
   ELSE DO TRANS:

      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "Cron"  
         ActionLog.KeyValue     = "" 
         ActionLog.ActionID     = "InvJournal"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.ActionDec    = liInvQty
         ActionLog.ActionChar   = "InvGroup: " + 
                                  (IF icInvGroup > ""
                                   THEN icInvGroup
                                   ELSE "ALL") + CHR(10) + 
                                  "Period: " +
                                  STRING(idtDate1,"99.99.9999") + "-" +
                                  STRING(idtDate2,"99.99.9999")
         ActionLog.ActionStatus = 3.
         ActionLog.ActionTS     = fMakeTS().

      /* file without the dir */
      lcPlainFile = lcFile.
      IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
         lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").
         
      ActionLog.KeyValue = lcPlainFile.
   END. 

END PROCEDURE.


