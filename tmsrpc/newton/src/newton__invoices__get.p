/**
 * Get invoice
 *
 * @input   invnum;integer;invoice sequence number
 * @output  array of invoice;invoice details
 * @invoice id;int;invoice number    
            amount;double;
            amount_paid;double;
            billing_period_from;datetime; 
            billing_period_to;datetime; 
            charge_type;string; 
            claim_status;string; 
            claim_status_description;string; 
            claim_allowed;boolean; 
            created_date;timestamp; 
            credit_invoice_number;int
            credit_invoice_number_external;string; 
            credit_note_status;int; 
            credit_reason_code;string;
            credit_reason_group;string;
            credit_reason_note;string;
            credit_user_id;string;
            customer_id;int; 
            delivery_type;string; 
            direct_debt_status;string; 
            display_invoice;boolean; 
            due_date;datetime; 
            duplicatable;int; 
            invoice_date;datetime; 
            invoice_number_external;string; 
            invoice_type;string; 
            msisdn;string; 
            msseq;int; 
            payment_date;datetime; 
            printing_status;string; 
            printing_denied;boolean; 
            tax_excluded;double; 
            tax_usage;string; 
            tax_amount;double; 
            tax_included;boolean; 
            amount_unpaid;double; 
            amount_interest;double; 
            amount_advance;double; 
            amount_credited;double; 
            amount_overpayment;double; 
            amount_doubtful;double; 
            customer_name;string; 
            customer_person_id;string; 
            payment_status;string; 
            sub_invoices;array of subinvoice;
 * @sub_invoice id;int;sub invoice id (unique per one invoice)
            msisdn;string;    
            paper;boolean; 
            credit_id;int; 
            amount;double; 
            credit_amount;double;
 */


{fcgi_agent/xmlrpc/xmlrpc_access.i}
{newton/src/json_key.i}

/* Output variables */
DEFINE VARIABLE lcInvType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcPrintState AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcClaimState AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcPaymState  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcVatUsage AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDDState AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCrExtID AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInvStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llDuplicatable AS LOGICAL NO-UNDO. 
DEFINE VARIABLE liDuplicatable AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcResult AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldDetCredit AS DECIMAL NO-UNDO.
DEFINE VARIABLE ldDetCLoss AS DECIMAL NO-UNDO.
DEFINE VARIABLE liDetCount AS INTEGER NO-UNDO.
DEFINE VARIABLE ldUnPaid  AS DECIMAL NO-UNDO.
DEFINE VARIABLE lcCustomerName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llMemoPresent  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lliSTC         AS LOGICAL   NO-UNDO.
DEFINE BUFFER bInv FOR Invoice.

DEF VAR pcId AS CHAR NO-UNDO. 
DEF VAR pcIdArray AS CHAR NO-UNDO. 
DEF VAR liCounter AS INTEGER NO-UNDO. 
DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "array") = ? THEN RETURN.
pcIDArray = get_array(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").

{Syst/commpaa.i}
gcBrand = "1".
katun = "NewtonRPC".
{Func/timestamp.i}            
{Func/finvbal.i}
{Func/fcreditvalid.i}
{Syst/tmsconst.i}
{Func/duplicate_invoice.i}


FUNCTION fAddCreditNote RETURN LOGICAL 
         (INPUT pcStruct AS CHAR,
          INPUT piSubInvoice AS INT,
          INPUT ldBal AS DECIMAL):

       DEFINE VARIABLE lcCheckError AS CHARACTER NO-UNDO.  
       DEFINE VARIABLE lcCreditMode AS CHARACTER NO-UNDO.
       /* default values */
       lcCheckError = "".
       
      /* check invoice */
      lcCheckError = fCheckInvoice(BUFFER Invoice, 
         (IF piSubInvoice > 0 THEN STRING(piSubInvoice) ELSE ""),
                                   "",
                                   OUTPUT lcCreditMode).
      IF lcCheckError > "" THEN DO:
         IF piSubInvoice > 0 AND
            lcCheckError BEGINS "SubInvoice has already been credited." THEN
            add_int(pcStruct,"credit_note_status",3). /* not possible */
         ELSE add_int(pcStruct,"credit_note_status",0). /* not possible */
         RETURN FALSE.
      END.
       
       /* check request */
       lcCheckError = fCheckCreditNoteRequest(Invoice.CustNum,
                                        Invoice.InvNum).
      IF lcCheckError > "" THEN DO:
          add_int(pcStruct,"credit_note_status",2). /* pending request */
          RETURN FALSE.
      END.

      /* Added "Invoice.ChargeType NE 5" due to YTS-7460 */
      IF ldBal NE Invoice.InvAmt AND        
         CAN-FIND(FIRST Payment OF Invoice) AND 
           (Invoice.InvType > 1 OR 
           (Invoice.ChargeType NE 2 AND
            Invoice.ChargeType NE 5)) THEN DO:
          lcCheckError = "Invoice has already been " +
                         (IF Invoice.PaymState = 1
                          THEN "partly " ELSE "") + "paid. Function not allowed". 
          add_int(pcStruct,"credit_note_status",0). /* not allowed */
          RETURN FALSE. 
      END.


      /* at this point credit note is allowed */
      add_int(pcStruct,"credit_note_status",1).
          
     RETURN TRUE.

END FUNCTION.

FUNCTION fAddSubInvoices RETURNS LOGICAL 
         (INPUT pcStruct AS CHAR,
          OUTPUT pliSTC  AS LOG):

   DEF VAR lcSubInvoices AS CHAR NO-UNDO.
   DEF VAR lcSubInvoice AS CHAR NO-UNDO.
   DEF VAR ldeInvRowVATAmt AS DEC NO-UNDO.
   DEF VAR ldeCreditAmt    AS DEC NO-UNDO.
   DEF VAR ldeInvPeriodStart AS DEC NO-UNDO.
   DEF VAR ldeInvPeriodEnd   AS DEC NO-UNDO.

   ASSIGN ldeInvPeriodStart = fMake2Dt(Invoice.FromDate,0)
          ldeInvPeriodEnd   = fMake2Dt(Invoice.ToDate,86399).

   lcSubInvoices = add_array(pcStruct,"sub_invoices").

   FOR EACH SubInvoice NO-LOCK WHERE
            SubInvoice.InvNum = Invoice.InvNum:

      ASSIGN ldeCreditAmt = 0
             ldeInvRowVATAmt = 0.

      /* Check iSTC happened on any subs. of this invoice */
      IF NOT pliSTC AND Invoice.InvType = 1 THEN
         FIND FIRST MsOwner WHERE
                    MsOwner.MsSeq  = SubInvoice.MsSeq  AND
                    MsOwner.TsBeg <= ldeInvPeriodEnd   AND
                    MsOwner.TsEnd >= ldeInvPeriodStart AND
                    MsOwner.PayType = FALSE            AND
                    MsOwner.CLIEvent BEGINS "iS" NO-LOCK NO-ERROR.
         IF AVAIL MsOwner AND MsOwner.TsBeg >= ldeInvPeriodStart AND
            MsOwner.TsBeg <= ldeInvPeriodEnd THEN pliSTC = TRUE.

      FIND FIRST SubSer NO-LOCK USE-INDEX ServPac WHERE 
            SubSer.MsSeq = SubInvoice.MsSeq AND 
            SubSer.ServPac = "TMSService" AND 
            SubSer.ServCom = "CALLSPEC" AND 
            SubSer.SSDate <= TODAY NO-ERROR.

      lcSubInvoice = add_struct(lcSubInvoices,"").
      
      add_int(lcSubInvoice,"id",SubInvoice.SubInvNum).
      add_string(lcSubInvoice,"msisdn",SubInvoice.CLI).
      add_boolean(lcSubInvoice,"paper", (
         IF AVAIL SubSer AND SubSer.SSStat = 1 THEN TRUE ELSE FALSE)).
      add_int(lcSubInvoice,"credit_id", SubInvoice.CrInvNum).
      add_double(lcSubInvoice,"amount", SubInvoice.InvAmt).
      fAddCreditNote(lcSubInvoice, SubInvoice.SubInvNum, ldUnPaid).

      /* Return Left SubInvoice Amount after partial credit note */
      /* so that we can make credit note for remaining amount    */
      FOR EACH InvRow OF SubInvoice NO-LOCK:
          IF InvRow.CreditInvNum = 0 THEN
             ASSIGN ldeInvRowVATAmt = ((InvRow.Amt * InvRow.VATPerc) / 100)
                    ldeInvRowVATAmt = ROUND(ldeInvRowVATAmt,2)
                    ldeCreditAmt = ldeCreditAmt + InvRow.Amt + ldeInvRowVATAmt.
      END. /* FOR EACH InvRow OF SubInvoice NO-LOCK: */
      add_double(lcSubInvoice,"credit_amount", ldeCreditAmt).

   END.

END FUNCTION.

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   FIND Invoice WHERE
        Invoice.Brand = "1" AND
        Invoice.InvNum = INT(pcId) NO-LOCK NO-ERROR.

   IF NOT AVAILABLE Invoice THEN DO:  
      RETURN appl_err("Invoice not found: " + pcId).
   END.
 
   FIND Customer WHERE 
        Customer.CustNum = Invoice.CustNum NO-LOCK NO-ERROR.

   ASSIGN 
   lcInvType    = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "Invoice",
                                   "InvType",
                                   STRING(Invoice.InvType))
                                       
   lcPrintState = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "Invoice",
                                   "PrintState",
                                   STRING(Invoice.PrintState))
                 
   lcClaimState = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "Invoice",
                                   "ClaimStatus",
                                    Invoice.ClaimStatus)

   lcPaymState  = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "Invoice",
                                   "PaymState",
                                   STRING(Invoice.PaymState))
             
   lcCType      = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "Invoice",
                                   "ChargeType",
                                   STRING(Invoice.ChargeType))

   lcDType      = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "Invoice",
                                   "DelType",
                                   STRING(Invoice.DelType))

   lcVatUsage   = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "Invoice",
                                   "VatUsage",
                                   STRING(Invoice.VatUsage))

   lcDDState   = IF Invoice.DDBankAcc = ""
                 THEN ""
                 ELSE DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "Invoice",
                                       "DDState",
                                       STRING(Invoice.DDState))
   lcCrExtID   = "".

   ldUnPaid     = fInvBal(BUFFER Invoice,TODAY).
                             
   IF Invoice.CrInvNum > 0 THEN
      FOR FIRST bInv NO-LOCK WHERE
                bInv.InvNum = Invoice.CrInvNum:
         lcCrExtID = bInv.ExtInvID.
      END.

   ldDetCredit = fCreditedAmt(BUFFER Invoice,TODAY).
   ldDetCLoss  = fCredLossPaid(BUFFER Invoice,TODAY,OUTPUT liDetCount).

   lcInvStruct = add_json_key_struct(resp_array, "").

   /* Invoice Structure */
   add_int(lcInvStruct, "id", Invoice.InvNum).
   add_double(lcInvStruct,"amount", Invoice.InvAmt).
   add_double(lcInvStruct,"amount_paid", Invoice.PaidAmt).
   add_double(lcInvStruct,"amount_interest",Invoice.InterestAmt).
   add_double(lcInvStruct,"amount_advance",Invoice.AdvPaym).
   add_double(lcInvStruct,"amount_credited",ldDetCredit).
   add_double(lcInvStruct,"amount_overpayment",Invoice.OverPaym).
   add_double(lcInvStruct,"amount_doubtful",ldDetCLoss).
   add_double(lcInvStruct,"amount_unpaid",ldUnPaid).


   add_datetime(lcInvStruct,"billing_period_from",Invoice.FromDate).
   add_datetime(lcInvStruct,"billing_period_to",Invoice.ToDate).
   add_string(lcInvStruct,"charge_type",STRING(Invoice.ChargeType) + " " + lcCType).
   add_string(lcInvStruct,"claim_status",Invoice.ClaimStatus).

   IF lcClaimState NE "" THEN 
      add_string(lcInvStruct,"claim_status_description",lcClaimState).

   add_boolean(lcInvStruct,"claim_allowed",Invoice.ClaimPerm).
   add_timestamp(lcInvStruct,"created_date",Invoice.ChgStamp).

   IF Invoice.CrInvNum > 0 THEN DO:
      add_int(lcInvStruct,"credit_invoice_number",Invoice.CrInvNum). 
      add_string(lcInvStruct,"credit_invoice_number_external",lcCrextID).
      add_string(lcInvStruct,"credit_reason_code",Invoice.CreditReason).
      add_string(lcInvStruct,"credit_reason_note",Invoice.xxmemo[1]).

      /* Find Credit Reason Group */
      FIND TMSCodes WHERE
           TMSCodes.TableName = "CreditNote" AND
           TMSCodes.FieldName = "Reason"     AND
           TMSCodes.CodeValue = Invoice.CreditReason NO-LOCK NO-ERROR.
      IF AVAILABLE TMSCodes THEN
         add_string(lcInvStruct,"credit_reason_group",TMSCodes.CodeGroup).
      ELSE
         add_string(lcInvStruct,"credit_reason_group","").

      FOR EACH Memo WHERE Memo.Brand     = gcBrand         AND
                          Memo.CustNum   = Invoice.CustNum AND
                          Memo.HostTable = "Invoice"       AND
                          Memo.KeyValue  = STRING(Invoice.InvNum) NO-LOCK:
          IF Memo.MemoTitle = "Credited" THEN DO:
             llMemoPresent = TRUE.
             add_string(lcInvStruct,"credit_user_id",Memo.CreUser).
             LEAVE.
          END. /* IF Memo.MemoTitle = "Credited" THEN DO: */
      END. /* FOR EACH Memo WHERE Memo.Brand */
      IF NOT llMemoPresent THEN
         add_string(lcInvStruct,"credit_user_id","Credit").
   END. /* IF Invoice.CrInvNum > 0 THEN DO: */

   /* add credit note info */
   fAddCreditNote(lcInvStruct, 0, ldUnPaid).

   add_int(lcInvStruct, "customer_id", Invoice.custnum).
   FIND Customer WHERE 
        Customer.CustNum = Invoice.CustNum NO-LOCK NO-ERROR. 
   IF AVAIL Customer THEN DO:
      add_string(lcInvStruct,"customer_person_id",Customer.OrgId).
      lcCustomerName =  Customer.FirstName + " " +
                        Customer.CustName + " " +
                        Customer.Surname2.
      /* quick fix to YTS-520 (JS-ON), fix should be done to xml-rpc framework*/
      add_string(lcInvStruct,"customer_name",
         replace(lcCustomerName,"\\","\\\\")).

   END.

   add_string(lcInvStruct,"delivery_type", STRING(Invoice.DelType) + " " + lcDType).
   add_string(lcInvStruct,"direct_debt_status", lcDDState).
   add_boolean(lcInvStruct,"display_invoice",Invoice.WInvDisp AND Invoice.InvCfg[1] = FALSE).
   add_datetime(lcInvStruct,"due_date",Invoice.DueDate).


   llDuplicatable = fDuplicateInvoiceValidate(
      BUFFER Invoice,
      OUTPUT lcResult).
   IF lcResult = {&MSG_ONG_REQUEST} THEN liDuplicatable = 2.
   ELSE liDuplicatable = INT(llDuplicatable).
   add_int(lcInvStruct,"duplicatable",liDuplicatable).

   add_datetime(lcInvStruct,"invoice_date",Invoice.InvDate).
   add_string(lcInvStruct,"invoice_number_external",Invoice.ExtInvID).
   add_string(lcInvStruct,"invoice_type", STRING(Invoice.InvType) + " " +  lcInvType).
   add_int(lcInvStruct, "msseq", Invoice.msseq).
   add_datetime(lcInvStruct,"payment_date",Invoice.PaymDate).
   add_string(lcInvStruct,"printing_status",STRING(Invoice.PrintState) + " " + lcPrintState).
   add_boolean(lcInvStruct,"printing_denied",Invoice.InvCfg[1]).
   add_string(lcInvStruct,"tax_usage", STRING(Invoice.VatUsage) + " " + lcVatUsage).
   add_double(lcInvStruct,"tax_amount",Invoice.VATAmt).
   add_boolean(lcInvStruct,"tax_included",Invoice.VatIncl).
   add_double(lcInvStruct,"tax_excluded",Invoice.AmtExclVAT).
   add_string(lcInvStruct,"payment_status",STRING(Invoice.PaymState) + " " + lcPaymState).

   fAddSubInvoices(lcInvStruct,OUTPUT lliSTC).

   add_boolean(lcInvStruct,"iSTC",lliSTC).

END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
