/* ===========================================================================
 MODULE ........: CLI_prepaidrate.p
 APPLICATION ...: Ticket Master
 TASK ..........: Analyse prepaid CDRs
 CREATED .......: 27.12.04/aam (from man_rate)
 CHANGED .......: 
                  
 VERSION .......: M15
 ============================================================================*/
         
{Syst/commali.i}      
{Func/excel.i}
{Func/email.i}
{Func/cparam2.i}

{Func/fcustcnt.i}
{Func/func.i}
{Mm/cdrvar.i}
{Mm/ficora.i}
{Rate/chkbal2.i} 
{Rate/mobol_tt.i}
{Func/fmakeservice.i}
{Func/fservlimit.i}
{Func/fsubser.i}
{Func/detailseq.i}
{Rate/daycampaign.i}
DEF STREAM msg.


DEF INPUT PARAMETER icCLI    AS CHAR NO-UNDO.
DEF INPUT PARAMETER idtFrom  AS DATE NO-UNDO.
DEF INPUT PARAMETER idtTo    AS DATE NO-UNDO. 
DEF INPUT PARAMETER ilSilent AS LOG  NO-UNDO.

DEF buffer xxmobsub          for mobsub.
def buffer sub-cust          for Customer.

DEF VAR totalchanges    as dec no-undo.
DEF VAR llpulise        AS LOG NO-UNDO.
DEF VAR llPuliseAllowed AS LOG NO-UNDO.

DEF VAR CLI  LIKE MobCDR.CLI NO-UNDO.
DEF VAR bwild              AS LOG NO-UNDO.       
DEF VAR bsuw               AS C   NO-UNDO.
DEF VAR vatperc LIKE vatcode.vatperc.
DEF VAR xMsgFile AS CHAR NO-UNDO.
DEF VAR Country_Code       AS C  NO-UNDO.
DEF VAR mi-no              AS C  NO-UNDO.
DEF VAR slseq              AS I  NO-UNDO.
DEF VAR CallTimeStamp      AS DE NO-UNDO.
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
       
/* DEF VAR ctype   AS LO NO-UNDO FORMAT "New Calls/All Calls". */

{Rate/rating_ttcall.i}

DEF TEMP-TABLE  ttDetail NO-UNDO LIKE McdrDtl.

{Rate/onlinevar.i}
{Rate/ticketfunc.i}



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
DEF VAR lcSaldoFatime AS C  NO-UNDO.
DEF VAR llChanged AS LOG NO-UNDO.


/* {Func/fcustcnt.i} */


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

{Rate/rate_roamzone.i}
{Rate/mobcdr_rate.i}

FUNCTION fBCopy RETURNS LOGICAL.
   
   IF prepcdr.InvSeq    ne ttCall.InvSeq      OR 
      prepcdr.TariffNum ne ttCall.TariffNum   OR
      prepcdr.BillCode  ne ttCall.BillCode    OR 
      prepcdr.Cli       NE ttCall.Cli         OR
      prepcdr.Amount    ne ttCall.Amount      OR 
      prepcdr.ccn       ne ttCall.ccn         OR
      PrepCDR.ErrorCode NE ttCall.ErrorCode THEN DO:
            
      FIND CURRENT prepcdr EXCLUSIVE-LOCK.
      BUFFER-COPY ttCall to prepcdr.
   END.
END.



/* Default values */
ASSIGN 
  bbatch   = SESSION:BATCH
  cdate1   = idtFrom
  cdate2   = idtTo
  CLI      = icCLI.

IF NOT bbatch THEN bbatch = ilSilent.


/* convert DATE fields into CHAR */
ASSIGN
xdate1 = STRING(YEAR (cdate1),"9999") +
         STRING(MONTH(cdate1),"99")   +
         STRING(DAY  (cdate1),"99")
xdate2 = STRING(YEAR (cdate2),"9999") +
         STRING(MONTH(cdate2),"99")   +
         STRING(DAY  (cdate2),"99")
freenumbers = "10022,112,10068".


{Rate/tariff_tt.i}

DEF VAR b_btluok AS C  NO-UNDO.
DEF VAR r_dest   AS C  NO-UNDO.
DEF VAR ldestamp AS DE no-undo.

ldestamp = YEAR(cdate1)  * 10000 +
           MONTH(cdate1) * 100   +
           DAY(cdate1) .


fFillTT().

ETIME(YES).
Main:
REPEAT:

   DO:
   
      MobCDR: 
      FOR EACH PrepCDR  WHERE   
               PrepCDR.CLI     = icCLI  AND         
               PrepCDR.datest >= cdate1 AND
               PrepCDR.datest <= cdate2    
      TRANSACTION WITH FRAME MobCDR: 
       
          {Rate/prep_rate.i}
          
       
       
