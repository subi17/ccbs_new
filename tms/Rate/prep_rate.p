/* ===========================================================================
 MODULE ........: prep_rate.p
 APPLICATION ...: Ticket Master
 TASK ..........: 
 CREATED .......: 
 CHANGED .......: 
 VERSION .......: 
 ============================================================================*/
         
{Syst/commali.i} 
{Func/excel.i}
{Func/email.i}
{Func/cparam2.i}

{Func/fcustcnt.i}
{Func/func.p}
{Mm/cdrvar.i}
{Mm/ficora.i}
{Rate/chkbal2.i} 
{Rate/mobol_tt.i}
{Func/fmakeservice.i}
{Func/fservlimit.i}
{Func/fsubser.i}
{Func/detailseq.i}
{Rate/daycampaign.i}
{Rate/error_codes.i}

DEF STREAM msg.
         
DEF buffer xxmobsub  for mobsub.
DEF buffer xxmsowner  for msowner.
DEF buffer xxservicelcounter for servicelCounter.
def buffer inv-cust  for Customer.

DEF VAR totalchanges    as dec no-undo.
DEF VAR llpulise        AS LOG NO-UNDO.
DEF VAR llPuliseAllowed AS LOG NO-UNDO.

DEF VAR CLI  LIKE PrepCDR.CLI NO-UNDO.
DEF VAR bwild              AS LOG NO-UNDO.       
DEF VAR bsuw               AS C   NO-UNDO.
DEF VAR vatperc LIKE vatcode.vatperc.
DEF VAR xMsgFile AS CHAR NO-UNDO.
DEF VAR Country_Code       AS C  NO-UNDO.
DEF VAR mi-no              AS C  NO-UNDO.
DEF VAR slseq              AS I  NO-UNDO.
DEF VAR CallTimeStamp      AS DE NO-UNDO.
DEF VAR ErrorCode1           AS I  NO-UNDO FORMAT "zzz9".
DEF VAR ErrorCode2           AS I  NO-UNDO.
DEF VAR cust-no1           AS I  NO-UNDO FORMAT "zzzzzzzz9".
DEF VAR cust-no2           AS I  NO-UNDO FORMAT "zzzzzzzz9".
DEF VAR cdate1             AS DA NO-UNDO.
DEF VAR cdate2             AS DA NO-UNDO.
DEF VAR xdate1             AS C  NO-UNDO.
DEF VAR xdate2             AS C  NO-UNDO.
DEF VAR saldotyyppi        AS C NO-UNDO.
DEF VAR attfile            AS C     NO-UNDO.
DEF VAR ig-code            like invgroup.invgroup no-undo.

DEF VAR SMS_prefix         AS C  NO-UNDO.
DEF VAR OWNGR_prefix       AS C  NO-UNDO.
DEF VAR SPEC_prefix        AS C  NO-UNDO.
DEF VAR INTL_prefix        AS C  NO-UNDO.
DEF VAR ISMS_prefix        AS C  NO-UNDO.
DEF VAR indeksi            AS C NO-UNDO.

DEF VAR b_PNP              AS LO NO-UNDO.
DEF VAR b_dest             AS C  NO-UNDO.
DEF VAR b_ccn              AS I  NO-UNDO.
DEF VAR b_prodcode         AS C  NO-UNDO.
DEF VAR b_dg-code          AS C  NO-UNDO.
DEF VAR b_foc              AS LO NO-UNDO.
DEF VAR b_pref             AS C  NO-UNDO.
DEF VAR b_asubtype         AS I  NO-UNDO.
DEF VAR lcCLIType          AS C  NO-UNDO. 
DEF VAR lcInvruncode       AS C  NO-UNDO.
       
/* DEF VAR ctype   AS LO NO-UNDO FORMAT "New Calls/All Calls". */

{Rate/rating_ttcall.i}

DEF TEMP-TABLE ttdetail no-undo like mcdrdtl.

{Rate/onlinevar.i}
{Rate/ticketfunc.i}
{Rate/rate_roamzone.i}
{Rate/mobcdr_rate.i}

DEF VAR ok         AS LO NO-UNDO FORMAT "Yes/No".
DEF VAR count      AS I  NO-UNDO.
DEF VAR TotValue   AS DE NO-UNDO.
DEF VAR MinBLen    AS I  NO-UNDO.
DEF VAR old_price  AS DE NO-UNDO.
DEF VAR new_price  AS DE NO-UNDO.
DEF VAR bbatch     AS LO NO-UNDO.
DEF VAR SL_prefix  AS C  NO-UNDO.
DEF VAR UseInvTarg AS C NO-undo.
DEF VAR data_prod  AS C  NO-UNDO.
DEF VAR IC_prefix  AS C  NO-UNDO.
DEF VAR OIC_prefix AS C  NO-UNDO.
DEF VAR setrates           AS I    NO-UNDO.
DEF VAR freenumbers        AS C    NO-UNDO FORMAT "x(79)".
def var lInvSeq            AS I    no-undo.
DEF VAR MobsubLimit        AS INT  NO-UNDO.
DEF VAR lcNotifyNumber     AS CHAR NO-UNDO.
DEF VAR CreditType         AS I    NO-UNDO.
DEF VAR errorcode          AS I    NO-UNDO.
DEF VAR llRateCCN          AS LOG  NO-UNDO INIT FALSE.

DEF BUFFER xMCDRfile FOR MCDRfile.

/* {ratebsub.i} */
DEF VAR pRBDest            AS C  NO-UNDO.
DEF VAR xConfDir           AS C  NO-UNDO.
DEF VAR ldpulserate        AS DE NO-UNDO.
DEF VAR liUnkCust          AS INT NO-UNDO.
DEF VAR ldeRoam%           AS DE  No-UNDO.

DEF VAR liPNPCCN      AS I   NO-UNDO.
DEF VAR liPNPRateCCN  AS I   NO-UNDO.
DEF VAR lcPNPPL       AS C   NO-UNDO.
DEF VAR lcTakeCLI     AS C   NO-UNDO. 
DEF VAR llChanged AS LOG NO-UNDO.

/* {Func/fcustcnt.i} */

DEF VAR lcSaldoFatime AS C  NO-UNDO.

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum AS INT
   INDEX CustNum CustNum.
   
{Func/tmsparam.i SaldoAgreementAccount  return}. lcSaldoFatime = tmsparam.CharVal.

{Func/cparam.i RepConfDir        return}.  xConfDir       = TmsParam.CharVAl.
{Func/cparam.i SL_prefix         return}.  SL_prefix      = TmsParam.CharVAl. /* 045 */
{Func/cparam.i DefIntlPref       return}.  INTL_Prefix    = TmsParam.CharVAl. /* 00  */
{Func/cparam.i DefPrefOwnGr      return}.  OWNGR_prefix   = TmsParam.CharVAl.
{Func/cparam.i DefPrefSpc        return}.  SPEC_prefix    = TmsParam.CharVAl.
{Func/cparam.i DefPrefSMS        return}.  SMS_prefix     = TmsParam.CharVAl.
{Func/cparam.i MinBillLen        return}.  MinBLen        = TmsParam.IntVal.
{Func/cparam.i DefPrefOSMS       return}.  ISMS_prefix    = TmsParam.CharVAl.
{Func/cparam.i AnalUsingInvT     return}.  Useinvtarg     = TmsParam.CharVAl.
{Func/cparam.i DefDataProd       return}.  data_prod      = TmsParam.CharVAl.
{Func/cparam.i IntCall_prefix    return}.  IC_prefix      = TmsParam.CharVAl. /*991*/
{Func/cparam.i IntOthCall_prefix return}.  OIC_prefix     = TmsParam.CharVAl. /*99*/
{Func/cparam.i ErrCDouble        return}.  errorcode      = TmsParam.IntVal.
{Func/cparam.i PulseRate         return}.  ldPulserate    = TmsParam.DecVal.
{Func/cparam.i UnknownCustomer   return}.  liUnkCust      = TmsParam.IntVal.
{Func/cparam.i Roaming%          return}.  ldeRoam%       = TmsParam.DecVal.

FUNCTION fBCopy RETURNS LOGICAL.
   
   IF prepcdr.InvSeq    ne ttCall.InvSeq    OR 
      prepcdr.TariffNum ne ttCall.TariffNum OR
      prepcdr.BillCode  ne ttCall.BillCode  OR 
      prepcdr.Cli       NE ttCall.Cli       OR
      prepcdr.Amount    ne ttCall.Amount    OR 
      prepcdr.ccn       ne ttCall.ccn       OR
      PrepCDR.ErrorCode NE ttCall.ErrorCode THEN DO:
            
      FIND CURRENT prepcdr EXCLUSIVE-LOCK.
      BUFFER-COPY ttCall to prepcdr.
   END.
END.


/* Default starts values */
ASSIGN 
  ErrorCode1 = 0
  ErrorCode2 = {&CDR_ERROR_NOT_ANALYSED_YET}
  bbatch =  session:batch
  cust-no1 = 0 cust-no2 = 999999999
  cdate2 = DATE(MONTH(TODAY),1,YEAR(TODAY)) .
  cdate1 = DATE(MONTH(cdate2),1,YEAR(CDATE2)).
  cdate2 =  DATE(MONTH(cdate1 + 45 ),1,
                  YEAR(cdate1 + 45)) - 1.
/* BBATCH Values */
IF bbatch THEN 
ASSIGN 
ErrorCode1 = {&CDR_ERROR_NOT_ANALYSED_YET}
ErrorCode2 = {&CDR_ERROR_NOT_ANALYSED_YET}
cdate1   = today - 50
cdate2   = today 
cust-no1 = 1
cust-no2 = 99999999.



form
skip(1)
"  Instruction:   This program recalculates call prices using "  skip
"                 current pricelists for all UNINVOICED calls."  skip(1)

"  Call dates ................:" cdate1 FORMAT "99-99-99"
HELP "Earliest call date" "-" cdate2 FORMAT "99-99-99"
HELP "Latest call date"
VALIDATE(INPUT cdate1 <= INPUT cdate2,"Invalid order of dates !")         SKIP

"  Invoice group .............:" ig-code
   invgroup.IGName at 42 format "x(28)"                                  skip

"  Customer Number ...........:" cust-no1
HELP "First Customer No. in range" "-"
cust-no2 FORMAT "zzzzzzzzz9"                  
HELP "Last Customer No. in range; APPLIES ONLY FOR RE-ANALYSIS"
VALIDATE(INPUT cust-no1 <= INPUT cust-no2,"Invalid order of customers !") SKIP
"  Invoice run code ..........:" lcInvrunCode FORMAT "X(2)" 
   HELP " EMPTY = all"                                                   SKIP

"  CLI Type ..................:" lcCLIType FORMAT "X(12)" 
   HELP "CLI type, EMPTY = all"
   VALIDATE(INPUT lcCLIType = "" OR
            CAN-FIND(CLIType WHERE 
                     CLIType.Brand = gcBrand AND
                     CLIType.CLIType = INPUT lcCLIType),
            "Unknown CLI type") 
   SKIP
"  CLI .......................:" cli
help "A-number, (empty = ALL A-numbers, * in the end = wildcard)"         SKIP
"  Error Code ................:" errorcode1
HELP "0 = All billable Calls"                                             SKIP
"  Re-analyse rateCCN ........:" llRateCCN
HELP "Do you want re-analyse Rating CCN using Calltype and Destination" 


 WITH  
   OVERLAY 
   ROW 1 
   WIDTH 80
   COLOR VALUE(cfc) 
   TITLE COLOR VALUE(ctc) 
    " " + ynimi + "   ANALYSE/RATE MOBILE CALLS   " + 
    string(pvm,"99.99.99") + " "  NO-LABELS  FRAME main.
   
PAUSE 0.


MAIN:
REPEAT WITH FRAME main:

IF NOT bbatch THEN DO:
   ehto = 9. RUN Syst/ufkey.
    
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
      RUN Syst/ufkey.
      
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

/* convert DATE fields into CHAR */
ASSIGN
xdate1 = STRING(YEAR (cdate1),"9999") +
         STRING(MONTH(cdate1),"99")   +
         STRING(DAY  (cdate1),"99")
xdate2 = STRING(YEAR (cdate2),"9999") +
         STRING(MONTH(cdate2),"99")   +
         STRING(DAY  (cdate2),"99")
freenumbers = "10022,112,10068" .

{Rate/tariff_tt.i}
DEF VAR b_btluok           AS C  NO-UNDO.
DEF VAR r_dest             AS C  NO-UNDO.

DEF VAR ldestamp AS DE no-undo.
DEF VAR ldebegStamp AS DE NO-UNDO.
ldestamp = YEAR(cdate1)  * 10000 +
           MONTH(cdate1) * 100   +
           DAY(cdate1) .

ldebegstamp = YEAR(cdate2)  * 10000 +
           MONTH(cdate2) * 100   +
           DAY(cdate2) .



fFillTT().

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

    lcTakeCLI = "".

   IF lcCLIType > "" AND lcTakeCLI = "" THEN NEXT.
   
   MobCDR: 
   FOR 
   EACH PrepCDR  WHERE   
        PrepCDR.CustNum  =  inv-cust.CustNum AND 
        PrepCDR.datest  >= cdate1            AND
        PrepCDR.datest  <= cdate2            AND 
       (if CLI ne "" then
        PrepCDR.CLI BEGINS CLI ELSE TRUE)  AND
       (if not bwild and CLI ne "" then CLI = PrepCDR.CLI     
        else if bwild then PrepCDR.CLI begins CLI else true)  AND               
       (IF lcCLIType > "" 
        THEN LOOKUP(PrepCDR.CLI,lcTakeCLI) > 0
        ELSE TRUE)                                           AND
       (if ErrorCode1 ne 0 then PrepCDR.ErrorCode = ErrorCode1
        ELSE TRUE)         
       TRANSACTION WITH FRAME MobCDR: 

       {Rate/prep_rate.i}
        
