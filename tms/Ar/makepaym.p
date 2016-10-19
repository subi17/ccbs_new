/* makepaym.p      23.10.2002/aam
   create payments

   changes:       
                    Eventlog in memo 13.11.2002/jr
                    22.11.2002/aam vat handling for credit loss
                    15.04.2003/aam return voucher nbr 
                    11.09.2003/aam brand
                    24.09.2004/aam use fInvBal; credit invoice's date may
                                   be in the future -> invbal.p calculates
                                   today's balance
                    12.01.05/aam CustNum to calcint.p
                    08.08.05/aam fCleanEventObjects (in lib/eventlog.i)
                    03.07.06/aam don't update PaymState if it is 4
                    12.01.07/aam CLI to fCustBal
                    23.04.07/aam ExtVoucher
                    30.08.07/aam customer balance handling
                    04.09.07/aam use createpaym.p
*/   

{commali.i}
{finvpayment.i}
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}
END.


DEF PARAMETER BUFFER PaidInv FOR Invoice.

DEF INPUT  PARAMETER idPaym      AS DEC  NO-UNDO.  /* paid amount */
DEF INPUT  PARAMETER idtDate     AS DATE NO-UNDO.  /* acc.& paym. date */
DEF INPUT  PARAMETER iiAccNum    AS INT  NO-UNDO.  /* account */
DEF INPUT  PARAMETER icSource    AS CHAR NO-UNDO.  /* payment source */
DEF INPUT  PARAMETER iiPaymType  AS INT  NO-UNDO.  /* payment type */
DEF INPUT  PARAMETER ilBehav     AS LOG  NO-UNDO.  /* update paym.behaviour */
DEF INPUT  PARAMETER ilInterest  AS LOG  NO-UNDO.  /* calculate interest */
DEF INPUT  PARAMETER icReference AS CHAR NO-UNDO.  /* reference for payment */
DEF INPUT  PARAMETER icMemo      AS CHAR NO-UNDO.  /* Memo text */
DEF OUTPUT PARAMETER oiVoucher   AS INT  NO-UNDO. 

DEF VAR llCustBal  AS LOG  NO-UNDO. 
DEF VAR ldPosting  AS DEC  NO-UNDO EXTENT 10.
DEF VAR liAccount  AS DEC  NO-UNDO EXTENT 10.

IF idtDate = ? THEN ASSIGN idtDate = TODAY.

FIND Customer OF PaidInv NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN "ERROR:Unknown customer".

FIND InvGroup where 
     InvGroup.Brand    = gcBrand AND 
     InvGroup.InvGroup = Customer.InvGroup NO-LOCK NO-ERROR.

ASSIGN 
   llCustBal    = IF AVAILABLE InvGroup
                  THEN InvGroup.UpdCustBal
                  ELSE FALSE
   ldPosting[1] = idPaym
   ldPosting[2] = -1 * idPaym
   liAccount[1] = iiAccNum
   liAccount[2] = PaidInv.ArAccNum.
   
RUN createpaym (PaidInv.CustNum,
                PaidInv.InvNum,
                PaidInv.CLI,
                idtDate,
                idtDate,
                ldPosting,
                liAccount,
                icSource,
                iiPaymType,
                icReference,
                icMemo,
                OUTPUT oiVoucher).

IF oiVoucher = 0 THEN 
   RETURN "ERROR:Payment creation failed. " + RETURN-VALUE.

IF llDoEvent AND 
   icSource  NE "DD" THEN DO:
   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER PaidInv:HANDLE.
   RUN StarEventInitialize(lhInvoice).
 
   RUN StarEventSetOldBuffer(lhInvoice).
END.

/* interest, balances etc. */
fInvoicePaymentUpdate(BUFFER PaidInv,
                      idtDate,
                      ldPosting[1],
                      oiVoucher,
                      ilInterest,
                      llCustBal,
                      ilBehav).

IF llDoEvent AND icSource  NE "DD" THEN 
   RUN StarEventMakeModifyEvent(lhInvoice). 

fCleanEventObjects().

RETURN "".



