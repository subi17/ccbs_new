/* rerate_define.i     11.03.11/aam separated from man_rate
*/

{Syst/commali.i}
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
{Rate/cdr_rerate.i}
{Rate/reratelog.i}
{Func/create_eventlog.i}

DEF VAR totalchanges       AS DEC NO-UNDO.
DEF VAR CLI  LIKE MobCDR.CLI NO-UNDO.
DEF VAR mi-no              AS C  NO-UNDO.
DEF VAR slseq              AS I  NO-UNDO.
DEF VAR CallTimeStamp      AS DE NO-UNDO.
DEF VAR cdate1             AS DA NO-UNDO.
DEF VAR cdate2             AS DA NO-UNDO.

DEF VAR b_PNP              AS LO NO-UNDO.
DEF VAR b_dest             AS C  NO-UNDO.
DEF VAR b_ccn              AS I  NO-UNDO.
DEF VAR b_prodcode         AS C  NO-UNDO.
DEF VAR b_dg-code          AS C  NO-UNDO.
DEF VAR b_foc              AS LO NO-UNDO.
DEF VAR b_pref             AS C  NO-UNDO.
DEF VAR b_asubtype         AS I  NO-UNDO.
       
{Rate/rating_ttcall.i}
DEF TEMP-TABLE  ttDetail NO-UNDO LIKE McdrDtl.

{Rate/onlinevar.i}
{Rate/ticketfunc.i}

DEF VAR ok              AS LO   NO-UNDO FORMAT "Yes/No".
DEF VAR count           AS I    NO-UNDO.
DEF VAR TotValue        AS DE   NO-UNDO.
DEF VAR old_price       AS DE   NO-UNDO.
DEF VAR new_price       AS DE   NO-UNDO.
DEF VAR bbatch          AS LO   NO-UNDO.
DEF VAR lInvSeq         AS I    NO-UNDO.
DEF VAR MobsubLimit     AS INT  NO-UNDO.
DEF VAR lcNotifyNumber  AS CHAR NO-UNDO.
DEF VAR CreditType      AS I    NO-UNDO.
DEF VAR errorcode       AS I    NO-UNDO.
DEF VAR liRerateSeq     AS INT  NO-UNDO.
DEF VAR r_dest          AS CHAR NO-UNDO.
DEF VAR ldestamp        AS DEC  NO-UNDO.
DEF VAR xConfDir        AS CHAR NO-UNDO.
DEF VAR ldpulserate     AS DEC  NO-UNDO.
DEF VAR liUnkCust       AS INT  NO-UNDO.
DEF VAR llChanged       AS LOG  NO-UNDO.
DEF VAR lcRerateSource  AS CHAR NO-UNDO. 
 
{Func/cparam.i RepConfDir      return}.   xConfDir      = TmsParam.CharVAl.
{Func/cparam.i ErrCDouble      return}.   errorcode     = TmsParam.IntVal.
{Func/cparam.i PulseRate       return}.   ldPulserate   = TmsParam.DecVal.
{Func/cparam.i UnknownCustomer return}.   liUnkCust     = TmsParam.IntVal.

{Rate/rate_roamzone.i}
{Rate/mobcdr_rate.i}
{Rate/rating_double_check.i}
{Rate/rerate_savecdr.i}

{Rate/tariff_tt.i}
{Rate/rating_package.i}

FUNCTION fEmptyRerateTempTables RETURNS LOG:
   EMPTY TEMP-TABLE ttServiceLCounter.
   EMPTY TEMP-TABLE ttDCCounter.
   EMPTY TEMP-TABLE ttSaldoCounter.
END.
