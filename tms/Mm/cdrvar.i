/* -------------------------------------------------------
  MODULE .......: CDRVAR.I
  FUNCTION .....: Common variables for FixCDR rating
  APPLICATION ..: TMS
  AUTHOR .......: KL
  CREATED ......: 26.06.03 kl
  MODIFIED .....: 09.09.03/aam lcRateBrand
                  31.05.04/aam ServRid & MPMRid

  VERSION ......: M15
--------------------------------------------------------------*/

/* CDR column variables */
DEF VAR c_version AS i  NO-UNDO.      /* CDR FORMAT version */
DEF VAR c_switch  AS c  NO-UNDO.      /* CDR creator switch id */
DEF VAR c_swname  AS c  NO-UNDO.      /* CDR creator switch name */
DEF VAR c_hc      AS i  NO-UNDO.      /* half call indicator */
DEF VAR c_reject  AS i  NO-UNDO.      /* indicates IF a call is rejected */
DEF VAR c_type    AS i  NO-UNDO.      /* type of call idetified BY switch */
DEF VAR c_atype   AS i  NO-UNDO.      /* type of CLI */
DEF VAR c_ctype   AS i  NO-UNDO.      /* type of routed number */
DEF VAR c_asub    AS c  NO-UNDO.      /* A-subscriber */
DEF VAR c_bsub    AS c  NO-UNDO.      /* B-subscriber */
DEF VAR c_csub    AS c  NO-UNDO.      /* C-subscriber */
DEF VAR c_fsub    AS c  NO-UNDO.      /* forwarding subscriber */
DEF VAR c_day     AS DA NO-UNDO.      /* DATE of call */
DEF VAR c_time    AS i  NO-UNDO.      /* call's beginning time in seconds */
DEF VAR c_dur     AS i  NO-UNDO.      /* duration of call */
DEF VAR c_tr_in   AS c  NO-UNDO.      /* trunk group IN  */
DEF VAR c_tr_out  AS c  NO-UNDO.      /* trunk group OUT */
DEF VAR c_op_in   AS c  NO-UNDO.      /* operator IN  */
DEF VAR c_op_out  AS c  NO-UNDO.      /* operator OUT   */
DEF VAR c_error   AS i  NO-UNDO.      /* error code */
DEF VAR c_pcm_in  AS i  NO-UNDO.      /* Pulse Code Modulation in */
DEF VAR c_cha_in  AS i  NO-UNDO.      /* Trunk Channel in */
DEF VAR c_pcm_out AS i  NO-UNDO.      /* Pulse Code Modulation out */
DEF VAR c_cha_out AS i  NO-UNDO.      /* Trunk Channel out */
DEF VAR c_retail  AS lo NO-UNDO.      /* retail OR wholesale */
DEF VAR c_bppref  AS c  NO-UNDO.      /* operator indirect prefix */
DEF VAR c_bptype  AS i  NO-UNDO.      /* indirect prefix type */
DEF VAR c_nochrg  AS i  NO-UNDO.      /* FreeOfCharge indicator */
DEF VAR c_custnr  AS i  NO-UNDO.      /* preanalysed customer number */
DEF VAR c_indir   AS lo NO-UNDO.      /* direct / indirect call */
DEF VAR tpos      AS i  NO-UNDO.      /* position FOR starting time in CDR */

DEF VAR BytesIn   AS I  NO-UNDO.
DEF VAR BytesOut  AS I  NO-UNDO.
DEF VAR UniIn     AS I  NO-UNDO.
DEF VAR UniOut    AS I  NO-UNDO.
DEF VAR NonUniIn  AS I  NO-UNDO.
DEF VAR NonUniOut AS I  NO-UNDO.
DEF VAR ErrorIn   AS I  NO-UNDO.
DEF VAR ErrorOut  AS I  NO-UNDO.

DEFINE TEMP-TABLE NewCDR NO-UNDO LIKE FixCDR.


DEF VAR ASubType     AS INT  NO-UNDO.
DEF VAR i            AS INT  NO-UNDO.
DEF VAR asub-cust    AS INT  NO-UNDO.
DEF VAR bsubs        AS CHAR NO-UNDO.
DEF VAR prbsubs      AS CHAR NO-UNDO.
DEF VAR rate-cust    AS INT  NO-UNDO.
DEF VAR x-time       AS INT  NO-UNDO.
DEF VAR bsub-prod    AS CHAR NO-UNDO.
DEF VAR rate-plcode  AS CHAR NO-UNDO.
DEF VAR bt-class     AS i    NO-UNDO.
DEF VAR liBillTarget AS INT  NO-UNDO. 
DEF VAR liCCN        AS INT  NO-UNDO. 
DEF VAR llVATIncl    AS LOG  NO-UNDO. 
DEF VAR lcRatePref   AS CHAR NO-UNDO. 
DEF VAR liDialType   AS INT  NO-UNDO. 
DEF VAR llCurrUnit   AS LOG  NO-UNDO. 
DEF VAR c_ptype      AS INT  NO-UNDO.
DEF VAR liRateType   AS INT  NO-UNDO.
DEF VAR base         AS DE   NO-UNDO.
DEF VAR bprice       AS DE   NO-UNDO FORMAT "999999.99999".
DEF VAR RowId        AS INT  NO-UNDO.
DEF VAR lcRateBrand  AS CHAR NO-UNDO. 
DEF VAR lcServRid    AS CHAR NO-UNDO.
DEF VAR lcMPMRid     AS CHAR NO-UNDO. 
DEF VAR lcOwnPrefix  AS CHAR NO-UNDO INIT "990,99590".
DEF VAR ldeTemp       AS DEC  NO-UNDO.
DEF VAR llFound       AS LOG  NO-UNDO.

DEF VAR liChangedQty    AS INT  NO-UNDO.
DEF VAR liCustChanged   AS INT  NO-UNDO. 
DEF VAR ldeGrossAmt     AS DEC  NO-UNDO.
DEF VAR ldeRoamarg      AS DEC  NO-UNDO.
DEF VAR llRoamInd       AS LOG  NO-UNDO.
DEF VAR llSaldoReminder AS LOG  NO-UNDO INIT FALSE. 
 



