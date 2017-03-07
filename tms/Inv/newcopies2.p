/* ----------------------------------------------------------------------
  MODULE .......: newcopies.p
  TASK .........: Prints copies of invoices, the limits of which have
                  been given in a source list, the code is based on the 
                  module printcopies.p
  APPLICATION ..: tms
  AUTHOR .......: vk
  CREATED ......: 15.01.08
  CHANGED ......: 21.01.08 vk based on printfromlist.p, but here all the
                              source lists have been combined to one, which
                              consists of MSISDN numbers and periods
                  17.04.08 vk The user has now a chance to devide the invoice                               
                              copies so, that each month will have an own copy                               
                              file. 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/ftransdir.i}

{Inv/printdoc1tt.i}

DEFINE INPUT PARAMETER pcMSISDNDir AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcInvoiceStatusDir AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcInvoiceStatisticsDir AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcTmpDir AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcInvoiceDir AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcMSISDNFile AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER plMonthlyFiles AS LOGICAL NO-UNDO. 
DEFINE OUTPUT PARAMETER piCombinationCount AS INTEGER NO-UNDO. 
DEFINE OUTPUT PARAMETER piCLICount AS INTEGER NO-UNDO. 
DEFINE OUTPUT PARAMETER piMonthCount AS INTEGER NO-UNDO. 

DEF STREAM sNAConf.
DEF STREAM sConfFound. 
DEF STREAM sNotFound.
DEF STREAM sStats.

DEF VAR icInvGrp    AS CHAR NO-UNDO.
DEF VAR iiCustNum1  AS INT  NO-UNDO.
DEF VAR iiCustNum2  AS INT  NO-UNDO.
DEF VAR icInvID1    AS CHAR NO-UNDO.
DEF VAR icInvID2    AS CHAR NO-UNDO.
DEF VAR iiInvDate   AS DATE NO-UNDO.
DEF VAR ilOnlyNew   AS LOGI NO-UNDO.
DEF VAR ilCredit    AS LOGI NO-UNDO.
DEF VAR iiInvType   AS INT  NO-UNDO.
DEF VAR icFile      AS CHAR NO-UNDO. 
DEF VAR ldtNameDate AS DATE NO-UNDO. 
DEF VAR lcDate      AS CHAR NO-UNDO.
DEF VAR ocError     AS CHAR NO-UNDO.
DEF VAR oiInvCount  AS INT  NO-UNDO.
DEF VAR tab         AS CHAR NO-UNDO.
DEF VAR lcLine      AS CHAR NO-UNDO.
DEF VAR lcCLI       AS CHAR FORMAT "X(9)" NO-UNDO. 
DEF VAR ldtAla      AS DATE NO-UNDO.
DEF VAR ldtYla      AS DATE NO-UNDO.
DEF VAR liOK        AS INT  NO-UNDO.
DEF VAR liW         AS INT  NO-UNDO.
DEF VAR ldtAla2     AS DATE NO-UNDO.
DEF VAR ldtYla2     AS DATE NO-UNDO.
DEF VAR liKuu       AS INT  NO-UNDO.     
DEF VAR liVuosi     AS INT  NO-UNDO. 
DEF VAR liPeriod    AS INT  NO-UNDO.                
DEF VAR liCompare   AS INT  NO-UNDO.
DEF VAR liOrigMonth AS INT  NO-UNDO.
DEF VAR lcExisting  AS CHAR NO-UNDO.
DEF VAR lcMissing   AS CHAR NO-UNDO.
DEF VAR lcNAConf    AS CHAR NO-UNDO.
DEF VAR liAjonKuu   AS INT  NO-UNDO.
DEF VAR liAjonPv    AS INT  NO-UNDO.
DEF VAR liValinta   AS INT  NO-UNDO.
DEF VAR lcAjonKuu   AS CHAR NO-UNDO.
DEF VAR lcAjonPv    AS CHAR NO-UNDO. 
DEF VAR lcStatistic AS CHAR NO-UNDO.
DEF VAR lcLastPer   AS CHAR NO-UNDO.

DEF VAR liProcessedCopies AS INTEGER NO-UNDO. 
DEF VAR liProcessedMonths AS INTEGER NO-UNDO. 
DEF VAR liProcessedCLIs   AS INTEGER NO-UNDO. 
DEF VAR pcDispComb AS CHARACTER FORMAT "X(11)" NO-UNDO. 
DEF VAR pcDispCLIs AS CHARACTER FORMAT "X(11)" NO-UNDO. 
DEF VAR pcDispMonths AS CHARACTER FORMAT "X(11)" NO-UNDO. 
DEF VAR pcMonth AS CHARACTER FORMAT "X(9)" NO-UNDO. 
DEF VAR icFileCall AS CHARACTER NO-UNDO. 

DEF TEMP-TABLE ttCopies NO-UNDO
  FIELD Period AS INT
  FIELD CLI    AS CHAR
  INDEX Period Period CLI.

DEF TEMP-TABLE ttProcessedCopy NO-UNDO
  FIELD Period AS INT
  FIELD CLI    AS CHAR
  INDEX Period Period CLI.



form
   " Month / Year : " pcMonth     " Current / Total: " pcDispMonths  skip
   " CLI          : " lcCLI      " Current / Total: " pcDispCLIs    skip
   " Combinations :             Current / Total: " pcDispComb          
WITH
   centered overlay no-labels title "Process status" 
   COLUMN 25 ROW 10 WIDTH 60 FRAME fStatus.


RUN pGatherInvoiceList.
RUN pProduceInvoiceFiles. 

FUNCTION fAddToProcessedCopies RETURN LOGICAL
   (INPUT piPeriod AS INTEGER, INPUT pcCLI AS CHARACTER):

   FIND FIRST ttProcessedCopy WHERE Period = piPeriod AND CLI = pcCLI NO-ERROR.
   IF NOT AVAILABLE ttProcessedCopy THEN
   DO:
      CREATE ttProcessedCopy.
      ASSIGN ttProcessedCopy.Period = piPeriod
             ttProcessedCopy.CLI = pcCLI.
      RETURN TRUE.
   END.
   RETURN FALSE.
END.

FUNCTION fExistProcessedCopy RETURN LOGICAL
   (INPUT piPeriod AS INTEGER, INPUT pcCLI AS CHARACTER):

   FIND FIRST ttProcessedCopy WHERE Period = piPeriod AND CLI = pcCLI NO-ERROR.
   IF AVAILABLE ttProcessedCopy THEN RETURN TRUE. ELSE RETURN FALSE.
END.

FUNCTION fExistProcessedPeriod RETURN LOGICAL (INPUT piPeriod AS INTEGER):
   FIND FIRST ttProcessedCopy WHERE Period = piPeriod NO-ERROR.
   IF AVAILABLE ttProcessedCopy THEN RETURN TRUE. ELSE RETURN FALSE.
END.

FUNCTION fExistProcessedCLI RETURN LOGICAL (INPUT pcCLI AS CHARACTER):
   FIND FIRST ttProcessedCopy WHERE CLI = pcCLI NO-ERROR.
   IF AVAILABLE ttProcessedCopy THEN RETURN TRUE. ELSE RETURN FALSE.
END.

FUNCTION fDispCounts RETURN LOGICAL:
    pcDispComb = STRING(liProcessedCopies) + " / " + STRING(piCombinationCount).
    pcDispCLIs  = STRING(liProcessedCLIs) + " / " + STRING(piCLICount).
    pcDispMonths = STRING(liProcessedMonths) + " / " + STRING(piMonthCount).

    pcMonth = STRING(liPeriod MOD 100) + " / " + 
              STRING(TRUNCATE(liPeriod / 100, 0)).
    DISP pcDispComb pcDispCLIs pcDispMonths pcMonth lcCLI 
       WITH FRAME fStatus.
END.


FUNCTION fMakeTemp RETURNS LOGICAL.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO. 
    DEFINE VARIABLE cNArow AS CHARACTER NO-UNDO. 
    lError = FALSE.

    cNArow = Invoice.CLI              + CHR(9) + 
             STRING(Customer.CustNum) + CHR(9) + 
             STRING(Invoice.InvNum)   + CHR(9).

    /* printing denied */
    IF Invoice.InvCfg[1] = TRUE THEN 
    DO:
       ocError = "Printing denied".
       lError = TRUE.
       cNARow = cNaRow + "No" + CHR(9).
    END.
    ELSE
        cNARow = cNaRow + "Yes" + CHR(9).

    IF ilOnlyNew AND Invoice.PrintState > 0 THEN 
    DO:
       cNARow = cNARow + "Yes" + CHR(9).
       lError = TRUE.
    END.
    ELSE
       cNARow = cNARow + "No" + CHR(9).
   
    IF NOT ilCredit AND Invoice.InvAmt < 0 THEN 
    DO:
       ocError = "Invoice is a credit invoice".
       lError = TRUE.
       cNARow = cNARow + "Yes" + CHR(9).
    END.
    ELSE
       cNARow = cNARow + "No"  + CHR(9).
    
    IF NOT ilCredit AND Invoice.CrInvNum > 0 AND Invoice.InvAmt >= 0 THEN 
    DO:
       ocError = "Invoice has been credited".
       lError = TRUE.
       cNARow = cNARow + "Yes" + CHR(9).
    END. 
    ELSE
       cNARow = cNARow + "No" + CHR(9).
    
    /* invoice type */
    IF iiInvType > 0 AND Invoice.InvType NE iiInvType THEN 
    DO:
       lError = TRUE.
       cNARow = cNARow + "No".
    END.
    ELSE
       cNARow = cNARow + "Yes".
   
    IF lError THEN
    DO:
       PUT STREAM sNAConf cNaRow SKIP.
       RETURN FALSE.
    END.
    ELSE 
    DO:
       CREATE ttInvoice.
       ASSIGN ttInvoice.InvNum = Invoice.InvNum
              oiInvCount       = oiInvCount + 1. 

       IF Customer.IDelName > "" THEN 
          ttInvoice.ZipCode = Customer.IDelZipCode.
       ELSE 
          ttInvoice.ZipCode = Customer.ZipCode.

       IF ldtNameDate = ? THEN ldtNameDate = Invoice.InvDate.
       RETURN TRUE.
    END.
END FUNCTION.


PROCEDURE pGatherInvoiceList:

   INPUT FROM VALUE(pcMSISDNDir + "/" + pcMSISDNFile).

   ASSIGN piCombinationCount = 0
          piCLICount         = 0
          piMonthCount       = 0.
   EMPTY TEMP-TABLE ttCopies.
   REPEAT:
       IMPORT UNFORMATTED lcLine.
       IF TRIM(lcLine) = "" THEN NEXT.

       lcCLI =            TRIM(ENTRY(1,lcLine,CHR(9))).
       liPeriod = INTEGER(TRIM(ENTRY(2,lcLine,CHR(9)))).

       IF NOT CAN-FIND(FIRST ttCopies WHERE CLI = lcCLI) THEN 
          piCLICount = piCLICount + 1. 
       IF NOT CAN-FIND(FIRST ttCopies WHERE Period = liPeriod) THEN 
          piMonthCount = piMonthCount + 1. 
          
          
       FIND FIRST ttCopies WHERE ttCopies.Period = liPeriod AND
                                 ttCopies.CLI    = lcCLI    
                                 EXCLUSIVE-LOCK NO-ERROR.
       IF NOT AVAIL ttCopies THEN DO:
           CREATE ttCopies.
           ASSIGN ttCopies.Period = liPeriod
                  ttCopies.CLI    = lcCLI.
           piCombinationCount = piCombinationCount + 1.
       END.

   END.
END.



PROCEDURE pProduceInvoiceFiles:
   ASSIGN liOK = 0
          liW  = 0
          liProcessedCopies = 0 
          liProcessedMonths = 0 
          liProcessedCLIs   = 0. 


   DEFINE VARIABLE liFirstPeriod AS INTEGER NO-UNDO.
   DEFINE VARIABLE liLastPeriod AS INTEGER NO-UNDO. 


   FIND FIRST ttCopies USE-INDEX Period NO-LOCK NO-ERROR.
   IF AVAIL ttCopies THEN 
   DO:
       ASSIGN liPeriod  = ttCopies.Period
              liCompare = ttCopies.Period.                         
   END.

   liFirstPeriod = 99999999.
   liLastPeriod = 0.
   
   FOR EACH ttCopies:
      IF liFirstPeriod > ttCopies.Period THEN liFirstPeriod = ttCopies.Period.
      IF liLastPeriod < ttCopies.Period  THEN liLastPeriod = ttCopies.Period.
   END.

   lcExisting = pcInvoiceStatusDir + "existing" + STRING(liPeriod) + ".txt".
   lcMissing  = pcInvoiceStatusDir + "missing" + STRING(liPeriod) + ".txt".

   lcExisting = fUniqueFileName(lcExisting, ".txt").
   lcMissing = fUniqueFileName(lcMissing, ".txt").

   OUTPUT STREAM sConfFound TO VALUE(lcExisting).
   OUTPUT STREAM sNotFound TO VALUE(lcMissing).   

   ASSIGN liAjonKuu = MONTH(TODAY) liAjonPv  = DAY(TODAY).

   IF liAjonKuu < 10 THEN 
      lcAjonkuu = "0" + STRING(liAjonKuu).
   ELSE 
      lcAjonKuu = STRING(liAjonKuu).

   IF liAjonPv  < 10 THEN 
      lcAjonPv  = "0" + STRING(liAjonPv).
   ELSE 
      lcAjonPv  = STRING(liAjonPv).
                          
   lcStatistic = pcInvoiceStatisticsDir + 
                 "invstats" + lcAjonKuu + lcAjonPv + ".txt".

   OUTPUT STREAM sStats TO VALUE(lcStatistic).

   ASSIGN icInvGrp   = ""
          iiCustNum1 = 0
          iiCustNum2 = 999999999
          icInvID1   = "" 
          icInvID2   = ""
          iiInvDate  = ?
          ilOnlyNew  = FALSE
          ilCredit   = TRUE
          iiInvType  = 0 
          tab        = CHR(124)
          gcBrand    = "1".
          
   PUT STREAM sConfFound "MSISDN"       CHR(9) 
                         "Customer nbr" CHR(9) 
                         "Invoice nbr"  CHR(9) 
                         "Amount" SKIP.
   PUT STREAM sNotFound "MSISDN" CHR(9) 
                        "Month of calls" SKIP.
   PUT STREAM sStats "Period"    CHR(9) 
                     "Successes" CHR(9) 
                     "Failures" SKIP.

   FOR EACH ttCopies USE-INDEX Period NO-LOCK:
       ASSIGN liPeriod = ttCopies.Period
              lcCLI    = ttCopies.CLI.
       IF (liPeriod NE liCompare) THEN 
       DO:
           /* The period has changed, so the previous period is ready, and */
           /* the current output files has to be closed and new ones to be */
           /* opened. */ 
           OUTPUT STREAM sNotFound CLOSE.
           OUTPUT STREAM sNAConf CLOSE.
           OUTPUT STREAM sConfFound CLOSE.
           
           /* liOK and liW give the correct and wrong for the last finished 
              period:  liComparePeriod */
           PUT STREAM sStats UNFORMATTED liCompare CHR(9) liOK CHR(9) liW SKIP.
           
           ASSIGN liOK   = 0  liW    = 0
                  icFile = "Dup" + STRING(liPeriod) + ".txt".

           lcExisting = pcInvoiceStatusDir + "existing" + STRING(liPeriod) + 
                        ".txt".
           lcMissing  = pcInvoiceStatusDir + "missing" + STRING(liPeriod) + 
                        ".txt".
           lcNAConf   = pcInvoiceStatusDir + "naconf"  + STRING(liPeriod) + 
                        ".txt".
           lcExisting = fUniqueFileName(lcExisting, ".txt").
           lcMissing = fUniqueFileName(lcMissing, ".txt").
           lcNAConf  = fUniqueFileName(lcNAConf, ".txt").

           OUTPUT STREAM sConfFound TO VALUE(lcExisting).
           PUT STREAM sConfFound "MSISDN" CHR(9) 
                                 "Customer nbr" CHR(9) 
                                 "Invoice nbr" CHR(9) 
                                 "Amount" SKIP.
           
           OUTPUT STREAM sNotFound TO VALUE(lcMissing). 
           PUT STREAM sNotFound "MSISDN" CHR(9) 
                                "Month of calls" SKIP.
          
           OUTPUT STREAM sNAConf TO VALUE(lcNAConf).
           PUT STREAM sNAConf "MSISDN" CHR(9) 
                              "Customer nbr" CHR(9) 
                              "Invoice nbr" CHR(9) 
                              "Print allowed" CHR(9) 
                              "PrintState disallowed " CHR(9)        
                              "Credit Invoice" CHR(9)
                              "Credited invoice" CHR(9) 
                              "InvType allowed". 

           IF plMonthlyFiles THEN 
           DO:
               icFile = pcTmpDir + "NewDup" + STRING(liCompare) + ".txt".
               icFileCall = pcInvoiceDir + "*" + icFile.
               RUN printdoc1.p(INPUT-OUTPUT TABLE ttInvoice,   
                             oiInvCount,
                             icFileCall,
                             "",
                             0,
                             0,
                             0,
                             OUTPUT oiInvCount).
               EMPTY TEMP-TABLE ttInvoice. 
           END.
           liCompare = liPeriod.
       END.
       
       liVuosi = TRUNCATE(liPeriod / 100,0).
       liKuu = liPeriod - 100 * liVuosi.
       liOrigMonth = liKuu.
       IF liKuu < 12 THEN 
          liKuu = liKuu + 1. 
       ELSE 
          ASSIGN liKuu   = 1  liVuosi = liVuosi + 1.
       
       ldtAla = DATE(liKuu,1,liVuosi) - 1.
       IF liKuu < 12 THEN 
          ldtYla = DATE(liKuu + 1,1,liVuosi).
       ELSE 
          ldtYla = DATE(        1,1,liVuosi + 1).

       FIND FIRST Invoice WHERE Invoice.Brand      = "1"    AND 
                                Invoice.CLI        = lcCLI  AND
                                Invoice.InvDate    > ldtAla AND
                                Invoice.InvDate    < ldtYla AND
                                Invoice.InvType    = 1      AND
                                Invoice.PrintState = 1      NO-LOCK NO-ERROR.
      
       DEFINE VARIABLE lOK AS LOGICAL NO-UNDO. 
       lOK = TRUE.

       IF NOT AVAIL Invoice THEN 
       DO:
           lOK = FALSE.
           PUT STREAM sNotFound UNFORMATTED 
               lcCLI       CHR(9) 
               liOrigMonth SKIP. 
       END.    
       IF AVAIL Invoice THEN 
       DO:
          PUT STREAM sConfFound UNFORMATTED 
             Invoice.CLI     CHR(9) 
             Invoice.CustNum CHR(9) 
             Invoice.InvNum  CHR(9) 
             Invoice.InvAmt  SKIP. 
       END. 

       FIND FIRST Customer WHERE Customer.CustNum = Invoice.CustNum 
           NO-LOCK NO-ERROR.
       IF NOT AVAIL Customer THEN lOK = FALSE.

       IF lOK THEN lOK = fMakeTemp().

       IF lOK THEN liOK = liOK + 1. ELSE liW  = liW  + 1.

       IF NOT fExistProcessedPeriod(liPeriod) THEN
          liProcessedMonths = liProcessedMonths + 1.
       IF NOT fExistProcessedCLI(lcCLI) THEN
          liProcessedCLIs = liProcessedCLIs + 1.

       IF NOT fExistProcessedCopy(liPeriod, lcCLI) THEN
       DO:
          liProcessedCopies = liProcessedCopies + 1.    
          fAddToProcessedCopies(liPeriod, lcCLI).
          fDispCounts().
       END.

   END.
   IF plMonthlyFiles THEN
      icFile     = pcTmpDir + "NewDup" + STRING(liPeriod) + ".txt".
   ELSE
      icFile     = pcTmpDir + "NewDup" + STRING(liFirstPeriod) + "_" + 
                   STRING(liLastPeriod) + ".txt". 
   icFileCall = pcInvoiceDir + "*" + icFile.
   RUN printdoc1.p(INPUT-OUTPUT TABLE ttInvoice,  
                 oiInvCount,
                 icFileCall,
                 "",
                 0,
                 0,
                 0,
                 OUTPUT oiInvCount). 

   ocError = RETURN-VALUE.

   OUTPUT STREAM sNotFound CLOSE.
   OUTPUT STREAM sConfFound CLOSE. 
   OUTPUT STREAM sNAConf CLOSE.

   IF liKuu = 1 THEN 
      ASSIGN liKuu = 12 liVuosi = liVuosi - 1.
   ELSE 
      liKuu = liKuu - 1. 

   IF liKuu < 10 THEN 
      lcLastPer = STRING(liVuosi) + "0" + STRING(liKuu).
   ELSE 
      lcLastPer = STRING(liVuosi) + STRING(liKuu).
                 
   PUT STREAM sStats UNFORMATTED lcLastPer CHR(9) liOK CHR(9) liW SKIP.

   OUTPUT STREAM sStats CLOSE.
END.
