/* -----------------------------------------------------------------------
  MODULE .......: dd_bankacc_change.p
  FUNCTION .....: Read in bank account changes for dd (CSB19, annex 5)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 30.09.09
  VERSION ......: Yoigo
 ---------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}
{Func/fbankdata.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
END.


DEF INPUT  PARAMETER  icFile     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER  oiRead     AS INT  NO-UNDO.
DEF OUTPUT PARAMETER  oiChanged  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER  oiErrors   AS INT  NO-UNDO.

DEF STREAM sRead.

DEF VAR liCustNum     AS INT  NO-UNDO. 
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcLine        AS CHAR NO-UNDO.
DEF VAR lcCompanyID   AS CHAR NO-UNDO.
DEF VAR lcLogFile     AS CHAR NO-UNDO.
DEF VAR lcFileCompany AS CHAR NO-UNDO.
DEF VAR lcCustName    AS CHAR NO-UNDO.
DEF VAR ldCurrStamp   AS DEC  NO-UNDO.
DEF VAR lcCurrentBank AS CHAR NO-UNDO.
DEF VAR lcNewBank     AS CHAR NO-UNDO.
DEF VAR liActionCode  AS INT  NO-UNDO.
DEF VAR lcPlainFile   AS CHAR NO-UNDO.
DEF VAR lcArcDir      AS CHAR NO-UNDO.
DEF VAR lcTransDir    AS CHAR NO-UNDO.

DEF STREAM sLog.


FUNCTION fWriteLog RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      liCustNum     "|"
      liActionCode  "|"
      lcCurrentBank "|"
      lcNewBank     "|"
      icMessage     SKIP.
     
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   oiErrors = oiErrors + 1.
   
   fWriteLog(icMessage).

   DO TRANS:
      /* save to db for reporting */
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "DDBankChg"
             ErrorLog.TableName = "Customer"
             ErrorLog.KeyValue  = STRING(liCustNum)
             ErrorLog.ActionTS  = ldCurrStamp
             ErrorLog.UserCode  = katun
             ErrorLog.ErrorMsg  = icMessage + CHR(10) +
                                  "New bank account: " + lcNewBank.
   END.

END FUNCTION.


/******* Main start ********/

IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".

RUN pInitialize.

RUN pReadChanges.

RUN pFinalize.

/******* Main end  ********/


PROCEDURE pInitialize:

   FIND FIRST Company NO-LOCK.
   ASSIGN 
      lcCompanyID = REPLACE(Company.CompanyID,"-","")
      ldCurrStamp = fMakeTS()
      oiRead      = 0
      lcLogFile   = fCParamC("DDBankChgLog")
      lcTransDir  = fCParamC("DDBankChgLogTrans")
      lcArcDir    = fCParamC("DDBankChgTrans").

   /* file without the dir */
   lcPlainFile = icFile.
   IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
      lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").

   IF lcLogFile = "" OR lcLogFile = ? THEN 
      lcLogFile = "/tmp/RESULTS.ANEX05.#DATE.DAT".
    
   lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(TODAY),"9999") + 
                                         STRING(MONTH(TODAY),"99")  +
                                         STRING(DAY(TODAY),"99")).

   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.
   
   IF llDoEvent THEN DO:
      lhCustomer = BUFFER Customer:HANDLE.
      RUN StarEventInitialize(lhCustomer).
   END.

END PROCEDURE.

PROCEDURE pReadChanges:

   INPUT STREAM sRead FROM VALUE(icFile).

   REPEAT:

      IMPORT STREAM sRead UNFORMATTED lcLine.

      /* customer lines */
      IF SUBSTRING(lcLine,1,4) = "5650" THEN DO:
       
         liCustNum = 0.
         
         ASSIGN 
            lcFileCompany = SUBSTRING(lcLine,5,9)
            liCustNum     = INTEGER(SUBSTRING(lcLine,17,12))
            lcCustName    = SUBSTRING(lcLine,29,40)
            lcNewBank     = SUBSTRING(lcLine,69,20)
            NO-ERROR.
     
         ASSIGN
            oiRead        = oiRead + 1
            lcCurrentBank = ""
            liActionCode  = 0.
 
         IF ERROR-STATUS:ERROR THEN DO:
            fError("Invalid data").
            NEXT.
         END.
      
         IF lcFileCompany NE lcCompanyID THEN DO:
            fError("Incorrect company ID").
            NEXT.
         END.
   
         FIND Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Customer THEN DO:
            fError("Unknown customer").
            NEXT.
         END.
      
         lcCurrentBank = Customer.BankAcc.
      
         IF LOOKUP(lcNewBank,",0") > 0 OR lcNewBank = FILL("0",20) THEN DO:
            fError("New bank account is empty").
            NEXT.
         END.
      
         IF Customer.ChargeType NE 2 THEN DO:
            fError("Customer's charge type is not direct debit").
            NEXT.
         END.

         IF NOT fCheckBankAcc(lcNewBank) THEN DO:
            fError("New bank account is not valid").
            NEXT.
         END.
         
         IF lcNewBank NE Customer.BankAcc THEN DO TRANS:

            FIND CURRENT Customer EXCLUSIVE-LOCK.
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).
            Customer.BankAcc = lcNewBank.
            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustomer).
            
            ASSIGN
               liActionCode = 1
               oiChanged    = oiChanged + 1. 

            fWriteLog("").
         END.

         ELSE DO:
            fWriteLog("New is same as current").
         END.

      END.  /* 5650=event line */
   
      /* end record */
      ELSE IF SUBSTRING(lcLine,1,4) = "5850" AND 
         lcCompanyID = SUBSTRING(lcLine,5,9)
      THEN DO:
   
         liCount = INTEGER(SUBSTRING(lcLine,117,10)) NO-ERROR.
     
         IF ERROR-STATUS:ERROR THEN NEXT.

         /* deduct header and end record */
         liCount = liCount - 2. 

         IF liCount NE oiRead THEN DO:
            liCustNum = 0.
            fWriteLog("NOTE:File totals do not match: " +
                      STRING(liCount) + " in end record  vrs  " +
                      STRING(oiRead) + " were read").
         END.
      END.     
        
   END.

   INPUT STREAM sRead CLOSE.

END PROCEDURE.

PROCEDURE pFinalize:

   OUTPUT STREAM sLog CLOSE.

   /* move to archive */
   IF oiRead > 0 AND lcArcDir > "" THEN DO:   
      fTransDir(icFile,
                "",
                lcArcDir).
   END.

   IF lcTransDir > "" THEN DO:
      fTransDir(lcLogFile,
                "",
                lcTransDir).
   END.
 
   DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "Customer"  
         ActionLog.KeyValue     = "CSB19" 
         ActionLog.ActionID     = "DDBankChg"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.ActionDec    = oiRead
         ActionLog.ActionChar   = lcPlainFile + CHR(10) +
                                  STRING(oiRead) + " lines read," + CHR(10) +
                                  STRING(oiChanged) + 
                                  " bank acc. changes done" +
                                  (IF oiErrors > 0 
                                   THEN ", " + CHR(10) + STRING(oiErrors) +
                                        " errors occurred"
                                   ELSE "")
         ActionLog.ActionStatus = 3
         ActionLog.UserCode     = katun.
         ActionLog.ActionTS     = fMakeTS().
   END.

   fCleanEventObjects().

END PROCEDURE.



