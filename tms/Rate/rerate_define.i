/* rerate_define.i     11.03.11/aam separated from man_rate
*/

{commali.i}
{cparam2.i}
{fcustcnt.i}
{func.i}
{cdrvar.i}
{ficora.i}
{chkbal2.i} 
{mobol_tt.i}
{fmakeservice.i}
{fservlimit.i}
{fsubser.i}
{detailseq.i}
{daycampaign.i}
{cdr_rerate.i}
{reratelog.i}
{create_eventlog.i}

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
       
{rating_ttcall.i}
DEF TEMP-TABLE  ttDetail NO-UNDO LIKE McdrDtl.

{onlinevar.i}
{ticketfunc.i}

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
 
{cparam.i RepConfDir      return}.   xConfDir      = TmsParam.CharVAl.
{cparam.i ErrCDouble      return}.   errorcode     = TmsParam.IntVal.
{cparam.i PulseRate       return}.   ldPulserate   = TmsParam.DecVal.
{cparam.i UnknownCustomer return}.   liUnkCust     = TmsParam.IntVal.

{rate_roamzone.i}
{mobcdr_rate.i}
{rating_double_check.i}
{rerate_savecdr.i}

{tariff_tt.i}
{rating_package.i}

FUNCTION fEmptyRerateTempTables RETURNS LOG:
   EMPTY TEMP-TABLE ttServiceLCounter.
   EMPTY TEMP-TABLE ttDCCounter.
   EMPTY TEMP-TABLE ttSaldoCounter.
END.
