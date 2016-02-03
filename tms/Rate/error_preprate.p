/*===========================================================================
 MODULE ........: analrate.p
 APPLICATION ...: Ticket Master
 TASK ..........: Analyse Mobile CDRs
 CREATED .......: 11.10.99 pt
 CHANGED .......: 18.10.99 jpo  Remove ctype (new calls/all calls) variable
                                Add calls with error codes 
                  26.10.99 pt   set mobsub.ms-alive
                  01.11.99 pt   more startup criteria
                  12.11.99 pt   more rules for CFO:unbillable
                  30.11.99 pt   cparam MinBillLen
                  14.12.99 jpo  unbilled calls total decreases, if call
                                will be analysed again. 
                  04.01.00 jpo  more rules for CFO:unbillable (begins 0701817) 
                  01.02.00 pt   find FIRST msowner ... (if multiple records
                                with overlapping time stamps
                  03.02.00 jpo  Set MobCDR.ms-seq
                  21.02.00 jpo  ms-rep
                  05.04.00 jpo  bbatch session
                  19.04.00 jpo  rated = false
                  25.10.00 jpo  invtarg is not optional (cparam)
                  02.11.00 jpo  Data Call get 'cparam' product code
                  29.11.00 jpo  Searching SSP & CFO calls
                  04.01.01 jpo  Palvelunumeroiden muutos (0800,9800)
                  21.03.01 jpo  Numerotiedustelun muuttui (@01118/@81118)
                  31.08.01 jpo  GSMOMA - speacial handling
                  02.01.02 jpo  WHEN spo-lis = 20 it can be sometimes 
                                billable calls
                  18.02.02 jpo  call forwarding is  free of charge             
                   
                  18.07.02 jpo   New for each routine 
                  26.09.02 jpo   Create invoice target, if not available
                  
                  30.10.02 jpo   old_ccn
                  20.11.02 jpo   pnpgroup
                  4.12.02  jpo   GPRS Rating
                  16.01.03 jpo   CallCount
                  13.03.03 jpo   +/991 prefix handling
                  11.04.03 jpo   fChkMobsubLimit
                  04.06.03 aam   mpm for international calls (echarge)
                  04.06.03 jpo   Roaming handling
                  05.06.03 jpo   Emergency handling
                  06.06.03 aam   roam-calls to errorcode 9007 
                  11.06.03 jp    CurrUnit
                  24.06.03 jp    VATincluded
                  25.06.03 aam   PNP
                  26.06.03 aam   cdrvar.i
                  28.08.03 jp    151-handling
                  24.10.03 jp    M15
                  04.01.05 aam CreditType, Balance and NotifyNumber from SubSer
                  11.01.07/aam fInvSeq to PrepCDR_rate.i
                  
 VERSION .......: M15
 ============================================================================*/

{Syst/commali.i}
{Func/excel.i}
{Func/email.i}
{Func/cparam2.i}
{Func/func.i}
{Func/fcustcnt.i}
{Rate/chkbal2.i}   
{Mm/cdrvar.i}
{Mm/ficora.i}
{Rate/mobol_tt.i}
{Func/fmakeservice.i}
{Func/fservlimit.i}
{Func/fsubser.i}
{Rate/rating_ttcall.i}

DEF TEMP-TABLE ttDetail LIKE Mcdrdtl.

{Func/detailseq.i}
{Rate/daycampaign.i}
{Rate/onlinevar.i}

DEF VAR CallTimeStamp      AS DE NO-UNDO.
{Rate/ticketfunc.i}

DEF INPUT PARAMETER   iiErrorCode AS INT  NO-UNDO.

DEF STREAM msg.


DEF buffer xxmobsub  for mobsub.
DEF buffer xxmsowner  for msowner.

DEF buffer  xxservicelcounter   for   servicelCounter.
DEF buffer xxxservicelcounter   for   servicelCounter.


def buffer inv-cust  for Customer.
DEF VAR CLI  LIKE PrepCDR.CLI NO-UNDO.
DEF VAR bwild                AS LOG NO-UNDO.       
DEF VAR bsuw                 AS C   NO-UNDO.
DEF VAR vatperc LIKE vatcode.vatperc.
DEF VAR xMsgFile AS CHAR NO-UNDO.
DEF VAR Country_Code         AS C  NO-UNDO.
DEF VAR mi-no                AS C  NO-UNDO.
DEF VAR slseq                AS I  NO-UNDO.
DEF VAR ErrorCode1           AS I  NO-UNDO FORMAT "zzz9".
DEF VAR ErrorCode2           AS I  NO-UNDO.
DEF VAR cust-no1             AS I  NO-UNDO FORMAT "zzzzzzzz9".
DEF VAR cust-no2             AS I  NO-UNDO FORMAT "zzzzzzzz9".
DEF VAR cdate1               AS DA NO-UNDO.
DEF VAR cdate2               AS DA NO-UNDO.
DEF VAR xdate1               AS C  NO-UNDO.
 DEF VAR xdate2              AS C  NO-UNDO.
DEF VAR saldotyyppi          AS C NO-UNDO.
DEF VAR attfile              AS C     NO-UNDO.
DEF VAR ig-code            like invgroup.invgroup no-undo.
DEF VAR totalchanges    AS DE  NO-UNDO.

DEF VAR SMS_prefix           AS C  NO-UNDO.
DEF VAR OWNGR_prefix         AS C  NO-UNDO.
DEF VAR SPEC_prefix          AS C  NO-UNDO.
DEF VAR INTL_prefix          AS C  NO-UNDO.
DEF VAR ISMS_prefix          AS C  NO-UNDO.
DEF VAR indeksi              AS C NO-UNDO.
DEF VAR SMS_orig             AS C   NO-UNDO.
DEF VAR SMS_term             AS C   NO-UNDO.
DEF VAR llRateCCN          AS LOG NO-UNDO INIT FALSE.

DEF VAR b_PNP                AS LO NO-UNDO.
DEF VAR b_dest               AS C  NO-UNDO.
DEF VAR b_ccn                AS I  NO-UNDO.
DEF VAR b_prodcode           AS C  NO-UNDO.
DEF VAR b_dg-code            AS C  NO-UNDO.
DEF VAR b_foc                AS LO NO-UNDO.
DEF VAR b_pref               AS C  NO-UNDO.
DEF VAR b_asubtype           AS I  NO-UNDO.

DEF VAR llpulise             AS LOG NO-UNDO.
DEF VAR llPuliseAllowed      AS LOG NO-UNDO.


DEF VAR prod_term AS C    NO-UNDO.
DEF VAR prod_orig AS C    NO-UNDO.
       
/* DEF VAR ctype   AS LO NO-UNDO FORMAT "New Calls/All Calls". */

DEF VAR lcnotifynumber       AS CH NO-UNDO.
DEF VAR ok                   AS LO NO-UNDO FORMAT "Yes/No".
DEF VAR count                AS I  NO-UNDO.
DEF VAR TotValue             AS DE NO-UNDO.
DEF VAR MinBLen              AS I  NO-UNDO.
DEF VAR old_price            AS DE NO-UNDO.
DEF VAR new_price            AS DE NO-UNDO.
DEF VAR bbatch               AS LO NO-UNDO.
DEF VAR SL_prefix            AS C  NO-UNDO.
DEF VAR UseInvTarg           AS C NO-undo.
DEF VAR data_prod            AS C  NO-UNDO.
DEF VAR IC_prefix            AS C  NO-UNDO.
DEF VAR OIC_prefix           AS C  NO-UNDO.
DEF VAR setrates             AS I  NO-UNDO.
DEF VAR freenumbers          AS C NO-UNDO FORMAT "x(79)".
def var lInvSeq              AS I   no-undo.
DEF VAR MobsubLimit          AS INT NO-UNDO.
DEF VAR CreditType           AS I   NO-UNDO.
DEF VAR errorcode            AS I   NO-UNDO.
DEF VAR llCreditLoss         AS LOG  NO-UNDO INIT "FALSE".

DEF BUFFER xMCDRfile FOR MCDRfile.

DEF VAR pRBDest              AS C   NO-UNDO.
DEF VAR xConfDir             AS C   NO-UNDO.
DEF VAR ldpulserate          AS DE  NO-UNDO.
DEF VAR liUnkCust            AS INT NO-UNDO.
DEF VAR ldeRoam%             AS DE  No-UNDO.
DEF VAr origperc             AS DE  NO-UNDO.
DEF VAR termPerc             AS DE  NO-UNDO.
DEF VAR roaminbill           AS I   NO-UNDO.

DEF VAR liPNPCCN             AS I   NO-UNDO.
DEF VAR lcPNPPL              AS C   NO-UNDO.
DEF VAR liPNPRateCCN         AS I   NO-UNDO.
DEF VAR liCLossCust          AS INT  NO-UNDO.
DEF VAR llChanged AS LOG NO-UNDO.

DEF VAR lcSaldoFatime AS C  NO-UNDO.

{Func/tmsparam.i SaldoAgreementAccount  return}. lcSaldoFatime = tmsparam.CharVal.

{Func/cparam.i RepConfDir        return}.  xConfDir        = TmsParam.CharVAl.
{Func/cparam.i SL_prefix         return}.  SL_prefix       = TmsParam.CharVAl. /* 045 */
{Func/cparam.i DefIntlPref       return}.  INTL_Prefix     = TmsParam.CharVAl. /* 00  */
{Func/cparam.i DefPrefOwnGr      return}.  OWNGR_prefix    = TmsParam.CharVAl.
{Func/cparam.i DefPrefSpc        return}.  SPEC_prefix     = TmsParam.CharVAl.
{Func/cparam.i DefPrefSMS        return}.  SMS_prefix      = TmsParam.CharVAl.
{Func/cparam.i MinBillLen        return}.  MinBLen         = TmsParam.IntVal.
{Func/cparam.i DefPrefOSMS       return}.  ISMS_prefix     = TmsParam.CharVAl.
{Func/cparam.i AnalUsingInvT     return}.  Useinvtarg      = TmsParam.CharVAl.
{Func/cparam.i DefDataProd       return}.  data_prod       = TmsParam.CharVAl.
{Func/cparam.i IntCall_prefix    return}.  IC_prefix       = TmsParam.CharVAl. /*991*/
{Func/cparam.i IntOthCall_prefix return}.  OIC_prefix      = TmsParam.CharVAl. /*99*/
{Func/cparam.i ErrCDouble        return}.  errorcode       = TmsParam.IntVal.
{Func/cparam.i UnknownCustomer   return}.  liUnkCust       = TmsParam.IntVal.
{Func/cparam.i  RoamOrigPerc     RETURN}.  origperc        = TMSparam.DecVal.
{Func/cparam.i  RoamTermPerc     RETURN}.  termperc        = TMSParam.DecVal.
{Func/cparam.i DEFPrefOSMS       RETURN}.  SMS_orig        = TmsParam.CharVal.
{Func/cparam.i DEFPrefTSMS       RETURN}.  SMS_term        = TmsParam.CharVal.
{Func/cparam.i RoaMinBillLen     RETURN}.  roaminbill      = TmsParam.IntVal.

ASSIGN liCLossCust   = fCParamI("CLossCustomer").

{Rate/rate_roamzone.i}
{Rate/mobcdr_rate.i}
{Rate/error_codes.i}

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
  cust-no1 = 999 cust-no2 = 999999999
  cdate2 = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
  cdate1 = DATE(MONTH(cdate2),1,YEAR(CDATE2)).

/* BBATCH Values */
IF bbatch THEN 
ASSIGN 
ErrorCode1 = {&CDR_ERROR_NOT_ANALYSED_YET}
ErrorCode2 = {&CDR_ERROR_NOT_ANALYSED_YET}
cdate1   = today - 50
cdate2   = today 
cust-no1 = 1
cust-no2 = 99999999.

ASSIGN
xdate1 = STRING(YEAR (cdate1),"9999") +
         STRING(MONTH(cdate1),"99")   +
         STRING(DAY  (cdate1),"99")
xdate2 = STRING(YEAR (cdate2),"9999") +
         STRING(MONTH(cdate2),"99")   +
         STRING(DAY  (cdate2),"99").
    
{Rate/tariff_tt.i}
DEF VAR b_btluok           AS C  NO-UNDO.
DEF VAR r_dest             AS C  NO-UNDO.

fFillTT().

ETIME(YES).

MAIN:
REPEAT WITH FRAME main:
     
CUSTOMER:
FOR FIRST inv-cust NO-LOCK.
   MobCDR: 
   FOR
   EACH PrepCDR  WHERE
        PrepCDR.errorcode = iiErrorCode 
        
       TRANSACTION WITH FRAME MobCDR: 

       {Rate/prep_rate.i} 
       
