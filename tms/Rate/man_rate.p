/* ===========================================================================
 MODULE ........: man_rate.p
 APPLICATION ...: Ticket Master
 TASK ..........: 
 CREATED .......: 
 CHANGED .......: 11.01.07/aam fInvSeq to mobcdr_rate.i
                  29.01.08 kl saldocounter.qty = 0
                  30.01.08 jp fBCopy
                  
 VERSION .......: xFera
 ============================================================================*/

&GLOBAL-DEFINE CounterHandling TempTable

{Syst/commali.i} 
{Rate/rerate_define.i}
{Rate/premiumnumber.i}

DEF VAR bwild              AS LOG NO-UNDO.       
DEF VAR lcInvruncode       AS C  NO-UNDO.
DEF VAR ErrorCode1         AS I  NO-UNDO.
DEF VAR cust-no1           AS I  NO-UNDO FORMAT "zzzzzzzz9".
DEF VAR cust-no2           AS I  NO-UNDO FORMAT "zzzzzzzz9".
DEF VAR ig-code            like invgroup.invgroup no-undo.
DEF VAR lcCLIType          AS C  NO-UNDO. 
DEF VAR llRateCCN          AS LOG NO-UNDO INIT FALSE.
DEF VAR ldebegStamp        AS DEC NO-UNDO.
DEF VAR lcTakeCLI          AS CHAR NO-UNDO.
DEF VAR liMsSeq            AS INT  NO-UNDO. 
       
DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum AS INT
   INDEX CustNum CustNum.

DEF BUFFER inv-cust  FOR Customer.
DEF BUFFER xxMsOwner FOR MsOwner.


/* Default start values */
ASSIGN 
  lcRerateSource = "UI" 
  ErrorCode1 = 0
  bbatch =  session:batch
  cust-no1 = 0 
  cust-no2 = 99999999
  cdate2 = DATE(MONTH(TODAY),1,YEAR(TODAY)) .
  cdate1 = DATE(MONTH(cdate2),1,YEAR(CDATE2)).
  cdate2 =  DATE(MONTH(cdate1 + 45 ),1,
                  YEAR(cdate1 + 45)) - 1.
/* BBATCH Values */
IF bbatch THEN ASSIGN 
   ErrorCode1 = {&CDR_ERROR_NOT_ANALYSED_YET}
   cdate1   = today - 50
   cdate2   = today 
   cust-no1 = 1.

form
   skip(1)
   "  Instruction:   This program recalculates call prices using "  skip
   "                 current pricelists for all UNINVOICED calls."  skip(1)

   "  Call dates ................:" 
      cdate1 FORMAT "99-99-99"  HELP "Earliest call date" "-" 
      cdate2 FORMAT "99-99-99"  HELP "Latest call date"
         VALIDATE(INPUT cdate1 <= INPUT cdate2,"Invalid order of dates !")
      SKIP

   "  Invoice group .............:" ig-code
      invgroup.IGName at 42 format "x(28)" skip

   "  Customer Number ...........:" 
      cust-no1 HELP "First Customer No. in range" "-"
      cust-no2 FORMAT "zzzzzzzzz9"                  
         HELP "Last Customer No. in range; APPLIES ONLY FOR RE-ANALYSIS"
         VALIDATE(INPUT cust-no1 <= INPUT cust-no2,
                  "Invalid order of customers !") SKIP

   "  Invoice run code ..........:" 
      lcInvrunCode FORMAT "X(2)" HELP " EMPTY = all" SKIP

   "  CLI Type ..................:" 
      lcCLIType FORMAT "X(12)" HELP "CLI type, EMPTY = all"
         VALIDATE(INPUT lcCLIType = "" OR
                  CAN-FIND(CLIType WHERE 
                           CLIType.Brand = gcBrand AND
                           CLIType.CLIType = INPUT lcCLIType),
                 "Unknown CLI type") SKIP
                 
    "  CLI .......................:" 
       cli
          help "A-number, (empty = ALL, * in the end = wildcard)" SKIP
   "  Error Code ................:" 
      errorcode1 HELP "0 = All billable Calls" SKIP
   "  Re-analyse rateCCN ........:" 
      llRateCCN
      HELP "Do you want re-analyse Rating CCN using Calltype and Destination"

WITH OVERLAY ROW 1 WIDTH 80 COLOR VALUE(cfc) 
   TITLE COLOR VALUE(ctc) 
      " " + ynimi + "   ANALYSE/RATE MOBILE CALLS   " + 
      string(pvm,"99.99.99") + " "  
   NO-LABELS FRAME main.
   
PAUSE 0.


MAIN:
REPEAT WITH FRAME main:

IF NOT bbatch THEN DO:
   ehto = 9. RUN Syst/ufkey.p.
    
 DISPLAY
"ALL" @ invgroup.IGName.

    
    UPDATE 
     cdate1   cdate2
    ig-code
    cust-no1 cust-no2
    lcInvrunCode
    lcCLIType
    CLI
    errorcode1
    llRateCCN
        
WITH FRAME main  EDITING:
      READKEY.
            
      IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME main:
         PAUSE 0.
         
         if frame-field = "ig-code" then do:
            assign ig-code.
            if ig-code = "" then do:
               display "ALL" @ invgroup.IGName.
            end.
            else do:
               find invgroup where 
                    InvGroup.Brand    = gcBrand AND
                    invgroup.Invgroup = ig-code no-lock no-error.
               if not avail invgroup then do:
                  bell.
                  message "Unknown invoicegroup !".
                  next.
               end.
               display invgroup.IGName.
            end.
         end.

         else if frame-field = "CLI" then do:
            assign CLI.
            if CLI ne "" and substr(CLI,length(CLI),1) = "*" then
               assign bwild = true CLI = substr(CLI,1,length(CLI) - 1).
            else bwild = false.
            if CLI = "" then .
            else do:
               if not bwild then
                  find first mobsub where MobSub.CLI  =     CLI
                  no-lock no-error.
               else
                  find first mobsub where MobSub.CLI BEGINS CLI
                  no-lock no-error.
               if not AVAIL mobsub then do:
                  bell.
                  message "Unknown Mobile Subscription !". 
                  message "Continue anyway ?" update ok.
                  if not ok then next.
               end.
               else if INPUT lcCLIType > "" AND
                       MobSub.CLIType NE INPUT lcCLIType 
               THEN DO:
                  bell.
                  MESSAGE 
                  "Subscription has a different CLI type than chosen one".
                  NEXT.
               END.
               
               else if INPUT lcInvruncode > "" AND
                       NOT CAN-FIND(FIRST INVrunlog WHERE 
                                          invrunlog.brand = "1"     AND   
                                          InvRunLog.date >= today   AND 
                                          InvRunLog.invcode  = 
                                          INT(INPUT lcinvruncode))
               THEN DO:
                  bell.
                  MESSAGE 
                  "WARNING! Unknown Invoice run code " INPUT lcInvruncode "!" .
                  NEXT.
               END.
               
            end.
         end.
         
         ELSE if frame-field = "cust-no1" then do:
              if input cust-no1 ne 0 THEN 
                 display input cust-no1 @ cust-no2 with frame main.
         end.
      END.    
      APPLY LASTKEY.
   END. /* EDITING */
        
   ACTION:
   REPEAT WITH FRAME main:
      ASSIGN
      ufk = 0 ehto = 0
      ufk[1] = 7 
      ufk[5] = 795
      ufk[8] = 8.
      RUN Syst/ufkey.p.
      
      IF toimi = 1 THEN NEXT  main.
      IF toimi = 8 THEN LEAVE main.
      IF TOIMI = 5 THEN DO:
         ok = false.
         MESSAGE "Do You REALLY want to START ANALYSIS RUN (Y/N) ?" UPDATE ok.
         IF NOT ok THEN NEXT action.
         ELSE LEAVE action.
      END.
   END. /* Action */      

END. /* bbatch */

ldestamp = YEAR(cdate1)  * 10000 +
           MONTH(cdate1) * 100   +
           DAY(cdate1) .

ldebegstamp = YEAR(cdate2)  * 10000 +
           MONTH(cdate2) * 100   +
           DAY(cdate2) .


fFillTT().

liRerateSeq = fRerateLogStart (
   katun,
   cdate1,
   cdate2,
   CLI,
   Cust-no1, /* custnum from */
   Cust-no2, /* custnum to */
   lcCLIType, /* clitype */
   ErrorCode1, /* ErrorCode */
   ig-code, /* InvGroup */
   ""). /* icInvRunCode*/

ETIME(YES).
      
CUSTOMER:
for each inv-cust no-lock where
         inv-cust.CustNum >= Cust-no1    and
         inv-cust.CustNum <= Cust-no2    and 
        (if lcinvruncode ne "" then
         inv-cust.Invcode = INT(lcInvruncode) 
         else true)                      AND 

        (if ig-code ne "" then
         inv-cust.Invgroup = ig-code 
         else true):

    ASSIGN
       lcTakeCLI = ""
       liMsSeq = 0.

    EMPTY TEMP-TABLE ttCust.
    
    fEmptyRerateTempTables().

    FOR EACH xxmsowner WHERE
             xxMsOwner.InvCust  = inv-cust.custnum  AND
             xxmsowner.tsend    >= ldestamp         AND
             xxMsowner.tsbegin  <= ldebegstamp      AND 
             xxMSOwner.Paytype   = FALSE            AND 
       (if cli ne "" then
             xxmsowner.cli  BEGINS cli ELSE TRUE)   AND
       (IF lcCLIType > "" 
        THEN xxmsowner.CLIType = lcCLIType 
        ELSE TRUE)  NO-LOCK:
    
       IF NOT CAN-FIND(FIRST ttCust WHERE ttCust.CustNum = xxMsOwner.CustNum)
       THEN DO:
          CREATE ttCust.
          ttCust.CustNum = xxMsOwner.CustNum.
       END.

       put screen row 11 string(ttcust.custnum).
        
       lcTakeCLI = lcTakeCLI + 
                   (IF lcTakeCLI > "" THEN "," ELSE "") +
                   xxmsowner.CLI.

       /* one selected */
       IF xxMsOwner.CLI = CLI THEN liMsSeq = xxMsOwner.MsSeq.

       /* counters to temp-table (with null amounts) */
       fServiceLCounter2Temp(xxMsOwner.MsSeq,
                             xxMsOwner.Custnum,
                             cDate1,
                             cDate2).

       fDCCounter2Temp(xxMsOwner.MsSeq,
                       cDate1,
                       cDate2).

       fSaldoCounter2Temp(xxMsOwner.MsSeq,
                          cDate1,
                          cDate2).

   END.   

   IF lcCLIType > "" AND lcTakeCLI = "" THEN NEXT.

   fInitializeRerateLog(inv-cust.CustNum,
                        liMsSeq,
                        CLI,
                        lcRerateSource,
                        cdate1,
                        cdate2).
   
   MobCDR: 
   FOR EACH ttCust,
   EACH MobCDR  NO-LOCK WHERE   
        MobCDR.CustNum  =  ttCust.CustNum AND 
        MobCDR.datest  >= cdate1          AND
        MobCDR.datest  <= cdate2          AND 
       (if CLI ne "" then
        MobCDR.CLI BEGINS CLI ELSE TRUE)  AND
       (if not bwild and CLI ne "" then CLI = MobCDR.CLI     
        else if bwild then MobCDR.CLI begins CLI else true)  AND               
       (IF lcCLIType > "" 
        THEN LOOKUP(MobCDR.CLI,lcTakeCLI) > 0
        ELSE TRUE)                                           AND
       (if ErrorCode1 ne 0 then MobCDR.ErrorCode = ErrorCode1
        ELSE TRUE)         
       TRANSACTION WITH FRAME MobCDR: 
       
       {Rate/man_rate2.i}
        
fRerateLogFinish(liRerateSeq).
