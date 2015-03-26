/* subinvdet.i 01.02.07/aam show SubInvoice details (from invdet.i)

   callers: payments.p
            nnasla.p
            claimhist.p
            nnlayp.p
*/


&IF "{&TMSCodeDef}" NE "NO"
&THEN
{commali.i}
&ENDIF

&IF "{&TimeStampDef}" NE "NO"
&THEN
{timestamp.i}
&ENDIF
            
DEF VAR lcCurrency   AS CHAR NO-UNDO.
DEF VAR lcCreditNote AS CHAR NO-UNDO.
DEF VAR lcVatUsage   AS CHAR NO-UNDO.
DEF VAR lcSubLine1   AS CHAR NO-UNDO.
DEF VAR lcSubLine2   AS CHAR NO-UNDO.

ASSIGN
   lcSubLine1 = FILL("-",35)
   lcSubLine2 = FILL("=",35).

FORM
    Invoice.ExtInvID   LABEL "Invoice Number " FORMAT "X(12)" 
       lcCreditNote LABEL "Credit Note" AT 42 FORMAT "X(12)"
       SKIP

    SubInvoice.SubInvNum LABEL "Subinvoice Nbr " FORMAT ">>>>>>>9"
       SKIP

    SubInvoice.CustNum      LABEL "User Customer ."                      
       Customer.CustName NO-LABEL FORMAT "x(25)"       SKIP

    SubInvoice.MsSeq        LABEL "Subscription ID" SKIP
    SubInvoice.CLI          LABEL "MSISDN ........"
       SKIP(1)

    SubInvoice.AmtExclVAT  LABEL "Total Excl. TAX" FORMAT "z,zzz,zz9.99+" 
       Invoice.VatUsage 
          LABEL "TAX Usage ..." AT 42
       lcVatUsage
          NO-LABEL
          FORMAT "X(20)"
       SKIP

    SubInvoice.VATAmt      LABEL "TAX Amount ...." FORMAT "z,zzz,zz9.99+" 
       Invoice.VatIncl AT 42
          LABEL "TAX Excl/Incl"
          FORMAT "Included/Excluded"
       SKIP

    lcSubLine1 
       NO-LABEL
       FORMAT "X(35)"
       SKIP

    SubInvoice.InterestAmt LABEL "Interest ......" FORMAT "z,zzz,zz9.99+" 
       SKIP

    SubInvoice.AdvPaym     LABEL "Adv. Payment .." FORMAT "z,zzz,zz9.99+" 
       SKIP

    SubInvoice.OverPaym     LABEL "Overpayment ..." FORMAT "z,zzz,zz9.99+" 
       SKIP

    SubInvoice.InvAmt   LABEL "TOTAL ........." FORMAT "z,zzz,zz9.99+" 
       lcCurrency   NO-LABEL  FORMAT "x(8)"      
       SKIP

    lcSubLine2 
       NO-LABEL
       FORMAT "X(35)"

    WITH  OVERLAY CENTERED ROW 3 SIDE-LABELS
       TITLE " VIEW SUBINVOICE DATA  (" + STRING(SubInvoice.InvNum) + ") "
       FRAME fInvDet.


PROCEDURE pSubInvoiceDetails:

   DEF INPUT PARAMETER iiInvNum AS INT NO-UNDO.
   DEF INPUT PARAMETER iiSubInv AS INT NO-UNDO.
   DEF INPUT PARAMETER ilHide   AS LOG NO-UNDO.

   DEF BUFFER bCreditInv FOR Invoice.
    
   FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK NO-ERROR.
   FIND SubInvoice WHERE 
        SubInvoice.InvNum    = iiInvNum AND 
        SubInvoice.SubInvNum = iiSubInv NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Invoice OR NOT AVAILABLE SubInvoice THEN DO:
      MESSAGE "Invoice information cannot be retrieved."
      VIEW-AS ALERT-BOX.
      RETURN.
   END. 

   FIND Customer WHERE 
        Customer.CustNum = SubInvoice.CustNum NO-LOCK NO-ERROR.

   ASSIGN
      lcCurrency   = Invoice.Currency
      lcCreditNote = ""
      lcVatUsage   = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "Invoice",
                                      "VatUsage",
                                      STRING(Invoice.VatUsage)).

   IF SubInvoice.CrInvNum > 0 THEN DO:
      FIND FIRST bCreditInv WHERE bCreditInv.InvNum = SubInvoice.CrInvNum
         NO-LOCK NO-ERROR.
      IF AVAILABLE bCreditInv THEN lcCreditNote = bCreditInv.ExtInvID.
   END.
   
   PAUSE 0.
   DISPLAY Invoice.ExtInvID 
           SubInvoice.SubInvNum
           lcCreditNote
           SubInvoice.CustNum 
           Customer.CustName + " " + Customer.FirstName 
              WHEN AVAILABLE Customer @ Customer.CustName
           SubInvoice.MsSeq
           SubInvoice.CLI 
           SubInvoice.AmtExclVAT lcCurrency
           lcSubLine1
           Invoice.VatUsage lcVatUsage Invoice.VatIncl
           SubInvoice.VATAmt  
           SubInvoice.InterestAmt   
           SubInvoice.AdvPaym SubInvoice.OverPaym     
           SubInvoice.InvAmt 
           lcSubLine2
   WITH FRAME fInvDet.

   IF ilHide THEN DO:
      MESSAGE "Press ENTER to continue".
      PAUSE NO-MESSAGE.

      HIDE FRAME fInvDet NO-PAUSE.
   END. 

END PROCEDURE. 

