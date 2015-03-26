/* -----------------------------------------------
  MODULE .......: HIGHUSAGECOL.P
  FUNCTION .....: Run MOBILE modules at 21:00 every evening
  APPLICATION ..: NNME
  AUTHOR .......: jp
  CREATED ......: 05.04.04
  MODIFIED .....: 29.11.04/aam no-lock for invseq,
                               foreach-loops rewritten,
                               use of InvCust added etc.
                  31.01.06/jl  moved to 17 different cron run
                  07.02.07 kl  use customer.birthday
                  22.03.07 kl  ldaFakeAge for companies
                  30.03.07/aam correct limits for age comparison
                  12.04.07/aam make requests for on demand invoices
                  10.05.07/aam cparam ODIRequestHS controls request creation
                  23.07.07 kl  delete invoicegroups records first
                  30.07.07 kl  delete records where mobsub does not exist

  VERSION ......: M15
------------------------------------------------------ */

{commali.i}
{eventlog.i}
{timestamp.i}
{cparam2.i}
{fmakemsreq.i}
{callquery.i}

DEF INPUT PARAMETER  icInvGroup AS CHAR       NO-UNDO.    

DEFINE VARIABLE date1        AS CHARACTER NO-UNDO.
DEFINE VARIABLE date2        AS CHARACTER NO-UNDO.
DEFINE VARIABLE tiednimi     AS CHARACTER NO-UNDO.
DEFINE VARIABLE kpl          AS INTEGER   NO-UNDO.
DEFINE VARIABLE dispkpl      AS INTEGER   NO-UNDO FORMAT "->>>>>>>>9".
DEFINE VARIABLE bbatch       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ldeBalance   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldaFrom      AS DATE      NO-UNDO.
DEFINE VARIABLE ldaTo        AS DATE      NO-UNDO.
DEFINE VARIABLE ldaCustDate  AS DATE      NO-UNDO.
DEFINE VARIABLE ldaFakeAge   AS DATE      NO-UNDO.
DEFINE VARIABLE ldODIStamp   AS DEC       NO-UNDO.
DEFINE VARIABLE ldODITime    AS DEC       NO-UNDO.
DEFINE VARIABLE lcResult     AS CHAR      NO-UNDO. 
DEFINE VARIABLE liCreateReq  AS INT       NO-UNDO. 
DEFINE VARIABLE tthCDR       AS HANDLE    NO-UNDO.


DEFINE TEMP-TABLE ttHighUsage NO-UNDO
   FIELD invseq   LIKE mobcdr.Invseq 
   FIELD CLI      LIKE MobCDR.CLI
   FIELD BillCode LIKE Mobcdr.BillCode 
   FIELD kpl      AS   INTEGER
   FIELD dur      LIKE MobCDR.BillDur
   FIELD price    LIKE MobCDR.amount
   INDEX InvSeq IS PRIMARY invseq CLI billcode
   INDEX cli cli billcode. 

DEFINE TEMP-TABLE ttMobSub NO-UNDO
   FIELD InvCust    AS     INT 
   FIELD msseq      LIKE   Mobsub.msseq 
   FIELD cli        LIKE   MobSub.cli
   FIELD product    LIKE   BillItem.BillCode
   FIELD limit      AS     DECIMAL
   FIELD cat        AS     CHAr
   INDEX InvCust IS PRIMARY InvCust cli product
   INDEX msseq InvCust MSSeq.

DEFINE TEMP-TABLE ttCust NO-UNDO
   FIELD InvCust AS INT
   INDEX InvCust InvCust.

DEFINE TEMP-TABLE ttHiUsagekat   NO-UNDO LIKE HiUsagekat.
DEFINE TEMP-TABLE ttHiUsagelimit NO-UNDO LIKE HiUsagelimit.
DEFINE TEMP-TABLE ttCall NO-UNDO LIKE MobCDR
   FIELD CDRTable AS CHAR.

DEFINE TEMP-TABLE ttTotalCLI NO-UNDO
   FIELD MsSeq  AS INT
   FIELD Amount AS DEC
   INDEX MsSeq MsSeq.

DEF BUFFER bUsage FOR HighUsage.


FOR EACH HiUsagekat no-lock.
    CREATE ttHiUsagekat.
    BUFFER-COPY HiUsagekat TO ttHiUsagekat.
end.

FOR EACH HiUsagelimit no-lock.
    CREATE ttHiUsagelimit.
    BUFFER-COPY HiUsagelimit TO ttHiUsagelimit.
end.


ASSIGN 
   date1       = STRING(YEAR (TODAY),"9999") +
                 STRING(MONTH(TODAY),"99")   +
                 STRING(DAY  (TODAY),"99")
   date2       = STRING(YEAR (TODAY),"9999") +
                 STRING(MONTH(TODAY),"99")   +
                 STRING(DAY  (TODAY),"99") 
   tiednimi    = fCParam("HighSpender","HighSpenderDirectory") +
                         "ttHighUsage_" + icInvGroup + ".txt"
   ldaFakeAge  = (IF MONTH(TODAY) = 2 AND DAY(TODAY) > 28
                  THEN DATE(2,28,YEAR(TODAY) - 200)
                  ELSE DATE(MONTH(TODAY),DAY(TODAY),YEAR(TODAY) - 200)).

ASSIGN 
   liCreateReq = fCParamI("ODIRequestHS")
   /* activation time for odi requests */
   ldODITime = fCParamDE("ODIRequestAct")
   tthCDR    = TEMP-TABLE ttCall:HANDLE.
   
IF liCreateReq = ? THEN liCreateReq = 0.   
   
IF ldODITime = ? OR ldOdiTime = 0 THEN ldODITime = 12.
ldODIStamp = fMake2Dt(TODAY,
                      INTEGER(TRUNCATE(ldODITime,0) * 3600 + 
                              100 * (ldODITime - TRUNCATE(ldODITime,0)) * 60)).


IF NOT SESSION:BATCH THEN DO:

   PUT SCREEN ROW 1 COL 15 "InvoiceGroup:" + icInvgroup.
   PUt SCREEN ROW 2 COL 15 "Started ....:" + string(today,"99-99-9999") + " " +
                                             string(time,"hh:mm:ss").  
   PUT SCREEN ROW 3 COL 15 
      " ---------------------------------------------------".

END.

/* delete all existing records where mobsub has terminated */
FOR EACH HighUsage NO-LOCK:
   IF NOT CAN-FIND(FIRST MobSub WHERE MobSub.CLI = HighUsage.CLI) THEN DO:
      FIND FIRST bUsage WHERE RECID(bUsage) = RECID(HighUsage) 
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAILABLE bUsage THEN DELETE bUsage.
   END.   
END.

/* delete existing records for this invoice group */
FOR EACH HighUsage NO-LOCK,
   FIRST MobSub    NO-LOCK WHERE
         MobSub.CLI = HighUsage.CLI,
   FIRST Customer NO-LOCK WHERE
         Customer.Brand    = gcBrand AND
         Customer.CustNum  = MobSub.CustNum AND
         Customer.InvGroup = icInvGroup:

   FIND FIRST bUsage WHERE RECID(bUsage) = RECID(HighUsage) EXCLUSIVE-LOCK.   
   DELETE bUsage.
END.

FOR EACH Customer NO-LOCK WHERE
         Customer.Brand    = gcBrand AND 
         Customer.invGroup = icInvGroup:
         
   FINDSUBS:      
   FOR EACH MobSub NO-LOCK WHERE
            MobSub.Brand   = gcBrand AND 
            Mobsub.InvCust = Customer.Custnum AND
            MobSub.PayType = FALSE:
         
   ldaCustDate = Customer.BirthDay.

   IF ldaCustDate = ? THEN ldaCustDate = ldaFakeAge.

   FOR EACH tthiusagekat NO-LOCK:

      IF tthiusagekat.clitype > "" AND 
         MobSub.clitype NE tthiusagekat.clitype THEN NEXT.
   
      IF tthiusagekat.ActInDays > 0 AND 
         MobSub.ActivationDate <= today - tthiusagekat.actindays 
      THEN NEXT.
   
      FOR EACH tthiusagelimit NO-LOCK WHERE
               tthiusagelimit.category = tthiusagekat.Category:

         ASSIGN
            ldaFrom = 01/01/1900
            ldaTo   = 01/01/2050
            kpl     = kpl + 1.
      
         IF NOT SESSION:BATCH AND kpl MOD 1000 = 0 THEN DO:
            PAUSE 0.
            DISP kpl LABEL "CLI" WITH 1 DOWN ROW 8 CENTERED.
         END. 

         IF ldaCustDate ne ? THEN ASSIGN
            ldaFrom  = (IF MONTH(TODAY) = 2 AND DAY(TODAY) > 28
                        THEN DATE(2,28,YEAR(TODAY) - tthiusagekat.ageto)
                        ELSE DATE(MONTH(TODAY),
                                  DAY(TODAY),
                                  YEAR(TODAY) - tthiusagekat.ageto))
            ldato  = (IF MONTH(TODAY) = 2 AND DAY(TODAY) > 28
                      THEN DATE(2,28,YEAR(TODAY) - tthiusagekat.agefrom)
                      ELSE DATE(MONTH(TODAY),
                                DAY(TODAY),
                                YEAR(TODAY) - tthiusagekat.agefrom)).

         IF ldaCustDate >= ldaFrom AND ldaCustDate <= ldaTo THEN DO:

            IF CAN-FIND(FIRST ttMobSub WHERE 
                              ttMobSub.InvCust = MobSub.InvCust   AND 
                              ttMobSub.cli     = MobSub.CLI       AND 
                              ttMobSub.Product = tthiusagelimit.BillCode) 
            THEN NEXT.                  

            CREATE ttMobSub.
            ASSIGN
               ttMobSub.InvCust = MobSub.InvCust
               ttMobSub.cli     = MobSub.CLI
               ttMobsub.MSSeq   = Mobsub.MSSeq 
               ttMobSub.Product = tthiusagelimit.BillCode
               ttMobSub.limit   = tthiusagelimit.Limit
               ttMobSub.cat     = tthiusagelimit.Cat.
               
            IF NOT CAN-FIND(FIRST ttCust WHERE 
                                  ttCust.InvCust = Customer.InvCust) THEN DO:

               CREATE ttCust.
               ttCust.InvCust = Customer.InvCust.
               
               dispkpl = dispkpl +  1.
            
               IF NOT SESSION:BATCH THEN
                  put screen row 5 col 15 "Total Customers:" + string(dispkpl).
            
            END.
               
            NEXT FINDSUBS.    
         END.
            
      END.

   END.
   END.
   
END.

OUTPUT STREAM excel TO  VALUE(tiednimi).
PUT STREAM excel UNFORMATTED STRING(TIME,"hh:mm:ss") " CDR" SKIP.
OUTPUT STREAM excel CLOSE.

kpl = 0.

FOR EACH ttCust:

   dispkpl = dispkpl - 1.
    
   IF NOT SESSION:BATCH THEN
      PUT SCREEN row 6  col 15 "Unprocessed Customers:" + STRING(dispkpl).
   
   FOR EACH ttMobSub WHERE
            ttMobSub.InvCust = ttCust.InvCust,
       EACH InvSeq NO-LOCK WHERE
            InvSeq.MsSeq = ttMobSub.MsSeq AND
            InvSeq.CustNum = ttMobSub.InvCust AND
            InvSeq.Billed = FALSE
   BREAK BY InvSeq.MsSeq
         BY InvSeq.ToDate:
      
         IF ttMobSub.product = "TOTAL" THEN DO:
         
            IF NOT FIRST-OF(InvSeq.ToDate) THEN NEXT. 
            
            FOR EACH SaldoCounter WHERE 
                     SaldoCounter.MSSeq  = ttMobsub.MSSEQ AND 
                     SaldoCounter.period =  YEAR(InvSeq.ToDate) * 100 + 
                                           MONTH(Invseq.ToDate) .
               kpl = kpl + 1.
   
               IF kpl MOD 10000 = 0 THEN DO:
                  IF NOT SESSION:BATCH THEN DO:
                     PAUSE 0.
                     DISP kpl LABEL "Mcdr" WITH 1 DOWN ROW 12 CENTERED.
                  END.
                  ELSE DO:
                     OUTPUT STREAM excel TO  VALUE(tiednimi) APPEND.
                     PUT STREAM excel UNFORMATTED
                        STRING(TIME,"hh:mm:ss") " " STRING(kpl) SKIP.
                     OUTPUT STREAM excel CLOSE.
                  END.   
               END.

               FIND FIRST ttHighUsage USE-INDEX InvSeq WHERE 
                          ttHighUsage.Invseq   = InvSeq.InvSeq     AND 
                          ttHighUsage.CLI      = ttMobsub.Cli      AND
                          ttHighUsage.BillCode = ttMobSub.Product
               NO-ERROR.

               IF NOT AVAIL ttHighUsage THEN DO:      
                  CREATE ttHighUsage.
                  ASSIGN 
                     ttHighUsage.CLI      = ttmobsub.CLI
                     ttHighUsage.InvSeq   = InvSeq.Invseq 
                     ttHighUsage.BillCode = ttMobSub.Product.
               END.   
         
               ASSIGN 
                  ttHighUsage.kpl   = ttHighUsage.kpl   + 1
                  ttHighUsage.price = ttHighUsage.price + SaldoCounter.amt.
            END.
         END.
         ELSE DO:
      
            RUN pCollectCDR (InvSeq.InvSeq,
                             InvSeq.FromDate,
                             InvSeq.ToDate).
            
            FOR EACH ttCall:
         
               IF ttCall.Billcode ne ttMobsub.Product then next.
             
               kpl = kpl + 1.
   
               IF kpl MOD 10000 = 0 THEN DO:
                  IF NOT SESSION:BATCH THEN DO:
                     PAUSE 0.
                     DISP kpl LABEL "Mcdr" WITH 1 DOWN ROW 12 CENTERED.
                  END.
                  ELSE DO:
                     OUTPUT STREAM excel TO  VALUE(tiednimi) APPEND.
                     PUT STREAM excel UNFORMATTED
                        STRING(TIME,"hh:mm:ss") " " STRING(kpl) SKIP.
                     OUTPUT STREAM excel CLOSE.
                  END.   

               END.

               FIND FIRST ttHighUsage USE-INDEX InvSeq WHERE 
                          ttHighUsage.Invseq   = ttCall.InvSeq  AND 
                          ttHighUsage.CLI      = ttCall.CLI     AND
                          ttHighUsage.BillCode = ttMobSub.Product
               NO-ERROR.

               IF NOT AVAIL ttHighUsage THEN DO:      
                  CREATE ttHighUsage.
                  ASSIGN 
                     ttHighUsage.CLI      = ttCall.CLI
                     ttHighUsage.InvSeq   = ttCall.Invseq 
                     ttHighUsage.BillCode = ttMobSub.Product.
               END.   
         
               ASSIGN 
                  ttHighUsage.kpl   = ttHighUsage.kpl   + 1
                  ttHighUsage.dur   = ttHighUsage.dur   + ttCall.BillDur
                  ttHighUsage.price = ttHighUsage.price + ttCall.amount.

            END.
   
         END.

   END.

END.      

OUTPUT STREAM excel TO  VALUE(tiednimi) APPEND.
PUT STREAM excel UNFORMATTED
   STRING(TIME,"hh:mm:ss") " HighUsage" SKIP.
OUTPUT STREAM excel CLOSE.
   
FOR EACH ttMobSub NO-LOCK,
    EACH ttHighUsage no-lock WHERE 
         ttHighUsage.cli      = ttMobSub.cli AND 
         ttHighUsage.BillCode = ttMobSub.product,
   FIRST InvSeq NO-LOCK WHERE
         InvSeq.InvSeq = ttHighUsage.InvSeq:

   if ttHighusage.price >  ttMobSub.limit  THEN DO:

      FIND FIRST HighUsage WHERE 
                 HighUsage.Invseq = ttHighUsage.InvSeq AND 
                 HighUsage.cli    = ttHighUsage.cli    NO-ERROR.

      IF not avail highusage THEN DO:
    
         CREATE highusage.
         ASSIGN 
            highusage.InvSeq           = ttHighUsage.InvSeq
            highusage.CLI              = ttHighUsage.cli
            highusage.Qty              = ttHighUsage.kpl
            highusage.Duration         = ttHighUsage.dur
            highusage.Amount           = ttHighUsage.price
            highusage.HiUsageStatus    = 0.
      
         ASSIGN
            highusage.CrStamp          = fmakets().
       
         ASSIGN
            highusage.ChStamp          = fmakets()
            highusage.Category         = ttMobSub.cat
            highusage.Date             = today
            highusage.date%            = 0
            highusage.DateGrow         = 0
            highusage.launch           = ttMobSub.product.
      END.
      ELSE IF today NE highusage.Date THEN DO:
       
         ASSIGN 
            highusage.date%    = (ttHighUsage.price - highusage.Amount) / 
                                    highusage.Amount * 100
            highusage.Date     = today
                                
            highusage.dategrow = ttHighUsage.price - highusage.Amount.                     
         ASSIGN 
            highusage.ChStamp          = fmakets()
            highusage.Qty              = ttHighUsage.kpl
            highusage.Duration         = ttHighUsage.dur
            highusage.Amount           = ttHighUsage.price.

      END.

      /* for on demand invoice check, current period's amount */
      IF InvSeq.ToDate >= TODAY AND liCreateReq > 0 THEN DO:
         FIND FIRST ttTotalCLI WHERE
                    ttTotalCLI.MsSeq = ttMobSub.MsSeq NO-ERROR.
         IF NOT AVAILABLE ttTotalCLI THEN DO:
            CREATE ttTotalCLI.
            ttTotalCLI.MsSeq = ttMobSub.MsSeq.
         END.
         ttTotalCLI.Amount = ttTotalCLI.Amount + ttHighUsage.Price.
      END.
      
   END.

END.   

/* should on demand invoices be made, compared against agreement customer's
   credit limit */
FOR EACH ttTotalCLI,
   FIRST MobSub NO-LOCK WHERE 
         MobSub.MsSeq = ttTotalCLI.MsSeq,
   FIRST Customer NO-LOCK WHERE
         Customer.CustNum = MobSub.AgrCust:
         
   IF ttTotalCLI.Amount > Customer.CreditLimit THEN DO:
      IF fODInvoiceRequest(MobSub.InvCust,
                           MobSub.MsSeq,
                           ldODIStamp,
                           FALSE,   /* create fees */
                           FALSE,   /* send sms */
                           "HighSpender",
                           OUTPUT lcResult)  = 0
      THEN DO:
         fELog("HIGHSPENDER/ODI","Request_failed:" + lcResult).
         
         /* save to db for reporting */
         CREATE ErrorLog.
         ASSIGN ErrorLog.Brand     = gcBrand
                ErrorLog.ActionID  = "ODIREQHS"
                ErrorLog.TableName = "MobSub"
                ErrorLog.KeyValue  = STRING(MobSub.MsSeq)
                ErrorLog.ErrorMsg  = "Creation failed: " + lcResult
                ErrorLog.UserCode  = katun.
                ErrorLog.ActionTS  = fMakeTS().
         
      END.
                           
   END.
   
END.

EMPTY TEMP-TABLE ttCall.
IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.

OUTPUT STREAM excel TO  VALUE(tiednimi) APPEND.
PUT STREAM excel UNFORMATTED
   STRING(TIME,"hh:mm:ss") " Done" SKIP.
OUTPUT STREAM excel CLOSE.
   

PROCEDURE pCollectCDR:

   DEF INPUT PARAMETER iiInvSeq    AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT PARAMETER idaToDate   AS DATE NO-UNDO.

   DEF VAR liErrorCodeOut AS INT  NO-UNDO.


   EMPTY TEMP-TABLE ttCall.

   fMobCDRCollect(INPUT "post",
                  INPUT gcBrand,
                  INPUT katun,
                  INPUT idaFromDate,   
                  INPUT idaToDate,
                  INPUT 0,
                  INPUT "",
                  INPUT "",
                  INPUT iiInvSeq,
                  INPUT 0,
                  INPUT "",
                  INPUT "",
                  INPUT "",
                  INPUT 0,
                  INPUT-OUTPUT liErrorCodeOut,
                  INPUT-OUTPUT tthCDR).

END PROCEDURE.


