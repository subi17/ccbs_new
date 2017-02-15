/**
 * Get Fusion invoice details
 *
 * @input fusion_invnum;int;mandatory;fusion invoice id
          hash;string;mandatory;hash value
 * @output fixed_invoice;struct;optional;Fixed line invoice (Telefonica)
           summary;struct;mandatory;Summary tariff data 
           mobile_invoice;struct;optional;Mobile line invoice (Yoigo)
           fusion_due_date;date;mandatory;Fusion invoice due date
 * @summary fixed_product_code;string;mandatory;Fixed product code (AG000000548264,AG000000548265,AG000000548266)
       fixed_tariff_mf;double;mandatory;Fusion tariff monthly fee
       mobile_tariff_type;string;mandatory;Mobile tariff type (CONTFF2,CONTSF13,CONTSF10)
       mobile_tariff_mf;double;mandatory;Mobile tariff monthly fee
 * @fixed_invoice product_code;string;mandatory;Product code #28
       product_text;string;mandatory;Base offer product text #29
       base_mf;double;mandatory;Base offer product monthly fee amount #33
       other_fees;double;mandatory;Other monthly fees #12-#33
       traffic;double;mandatory;Traffic amount #14
       other_items;double;mandatory;Other items amount #16
       taxable_income;double;mandatory;Taxable income #19
       tax_amount;double;mandatory;Tax amount #21
       total_amount;double;mandatory;Total invoice #23
       additional_items;double;mandatory;Additional items #27
       total_to_pay;double;mandatory;Total to pay #25
 * @mobile_invoice tariff_type;string;mandatory;CONTFF2,CONTSF13,CONTSF10
       base_mf;double;mandatory;
       other_fees;double;mandatory;
       traffic;double;mandatory;
       taxable_income;double;mandatory;Taxable income
       tax_amount;double;mandatory;Taxes
       total_amount;double;mandatory;Total invoice
       installment_amount;double;mandatory;billing items starting with PAYTERM
       permanency_penalty;double;mandatory;Permanency penalty fees
       total_to_pay;double;mandatory;Total to pay
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
katun = "NewtonRPC".
gcBrand = "1".
{Syst/tmsconst.i}
{Inv/fusioninvoice.i}

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR pcTenant AS CHAR NO-UNDO.
DEF VAR piFusionInvnum AS INT NO-UNDO. 
DEF VAR pcHash AS CHAR NO-UNDO. 

DEF VAR lcHash AS CHAR NO-UNDO. 
DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR lcFixedStruct AS CHAR NO-UNDO. 
DEF VAR lcSummaryStruct AS CHAR NO-UNDO. 
DEF VAR lcMobileStruct AS CHAR NO-UNDO. 

DEF VAR lcMobileTariffType AS CHAR NO-UNDO. 
DEF VAR ldeMobileTariffMF AS DEC NO-UNDO. 
DEF VAR lcFixedProductCode AS CHAR NO-UNDO. 
DEF VAR ldeFixedTariffMF AS DEC NO-UNDO. 
   
IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

validate_struct(pcStruct,"brand,fusion_invnum!,hash!").

ASSIGN
   pcTenant = get_string(pcStruct, "brand")  
   piFusionInvNum = get_int(pcStruct, "fusion_invnum")
   pcHash = get_string(pcStruct, "hash").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FIND FIRST FusionInvoice NO-LOCK WHERE
           FusionInvoice.FuInvNum = piFusionInvNum NO-ERROR.

IF NOT AVAIL FusionInvoice THEN
   RETURN appl_err("Fusion invoice not found").

IF FusionInvoice.InvNum > 0 THEN DO:
   FIND FIRST Invoice NO-LOCK WHERE
              Invoice.Invnum = FusionInvoice.InvNum NO-ERROR.
   IF NOT AVAIL Invoice THEN RETURN appl_err("Invoice not found").
END.

lcHash = fGenerateFusionInvoiceHash(
            FusionInvoice.InvNum,
            FusionInvoice.InvoiceNum,
            FusionInvoice.CustomerID).

IF pcHash NE lcHash THEN RETURN appl_err("Incorrect hash value").

lcResultStruct = add_struct(response_toplevel_id, "").

IF FusionInvoice.InvNum > 0 THEN
   add_datetime(lcResultStruct, "fusion_due_date",Invoice.DueDate).
ELSE
   add_datetime(lcResultStruct, "fusion_due_date",
      DATE(MONTH(FusionInvoice.InvDate), 16, YEAR(FusionInvoice.InvDate))).
   
IF FusionInvoice.InvNum > 0 THEN 
   lcMobileTariffType = FusionInvoice.MSubsType.

IF FusionInvoice.productCode > "" THEN
   lcFixedProductCode = FusionInvoice.ProductCode.

IF lcMobileTariffType EQ "" THEN
   CASE lcFixedProductCode:
      WHEN "AG000000548265" THEN lcMobileTariffType = "CONTFF2".
      WHEN "AG000000548266" THEN lcMobileTariffType = "CONTSF14".
      WHEN "AG000000548264" THEN lcMobileTariffType = "CONTSF10".
      OTHERWISE RETURN 
        appl_err(SUBST("Unsupported fixed_product_code &1",lcFixedProductCode)).
   END CASE.

IF lcFixedProductCode EQ "" THEN
   CASE lcMobileTariffType:
      WHEN "CONTFF2" THEN lcFixedProductCode = "AG000000548265".
      WHEN "CONTSF14" THEN lcFixedProductCode = "AG000000548266".
      WHEN "CONTSF10" THEN lcFixedProductCode = "AG000000548264".
      OTHERWISE RETURN 
        appl_err(SUBST("Unsupported mobile_tariff_type &1",lcMobileTariffType)).
   END CASE.

CASE lcMobileTariffType:
   WHEN "CONTFF2"  THEN ldeMobileTariffMF = 2.0.
   WHEN "CONTSF14" THEN ldeMobileTariffMF = 14.72.
   WHEN "CONTSF10" THEN ldeMobileTariffMF = 10.72.
END.

CASE lcFixedProductCode:
   WHEN "AG000000548265" THEN ldeFixedTariffMF = 34.48.
   WHEN "AG000000548266" THEN ldeFixedTariffMF = 36.76.
   WHEN "AG000000548264" THEN ldeFixedTariffMF = 50.76.
END.

lcSummaryStruct = add_struct(lcResultStruct,"summary").
add_string(lcSummaryStruct,"fixed_product_code", lcFixedProductCode).
add_double(lcSummaryStruct,"fixed_tariff_mf",ldeFixedTariffMF).
add_string(lcSummaryStruct,"mobile_tariff_type",lcMobileTariffType).
add_double(lcSummaryStruct,"mobile_tariff_mf",ldeMobileTariffMF).

IF FusionInvoice.ProductCode > "" THEN DO:
   lcFixedStruct = add_struct(lcResultStruct,"fixed_invoice").
   add_string(lcFixedStruct,"product_code",FusionInvoice.ProductCode).
   add_string(lcFixedStruct,"product_text",FusionInvoice.ProductText).
   add_double(lcFixedStruct,"base_mf",FusionInvoice.BaseServiceMF).
   add_double(lcFixedStruct,"other_fees",FusionInvoice.OtherMF).
   add_double(lcFixedStruct,"traffic",FusionInvoice.TrafficAmt).
   add_double(lcFixedStruct,"other_items",FusionInvoice.OtherItems).
   add_double(lcFixedStruct,"taxable_income",FusionInvoice.TaxIncome).
   add_double(lcFixedStruct,"tax_amount",FusionInvoice.TaxAmt).
   add_double(lcFixedStruct,"total_amount",FusionInvoice.TotalAmt).
   add_double(lcFixedStruct,"additional_items",FusionInvoice.AddItems).
   add_double(lcFixedStruct,"total_to_pay",FusionInvoice.TotalToPay).
END.
       
IF Fusion.InvNum > 0 THEN DO:
   lcMobileStruct = add_struct(lcResultStruct,"mobile_invoice").
   add_string(lcMobileStruct,"tariff_type",FusionInvoice.MSubsType).
   add_double(lcMobileStruct,"base_mf",FusionInvoice.MTariffMF).
   add_double(lcMobileStruct,"other_fees",FusionInvoice.MOtherMF).
   add_double(lcMobileStruct,"traffic",FusionInvoice.MTraffic).
   add_double(lcMobileStruct,"taxable_income",FusionInvoice.MTaxableIncome).
   add_double(lcMobileStruct,"tax_amount",FusionInvoice.MVatAmt).
   add_double(lcMobileStruct,"total_amount",FusionInvoice.MTotalInvoice).
   add_double(lcMobileStruct,"installment_amount",FusionInvoice.MTermFinancing).
   add_double(lcMobileStruct,"permanency_penalty",FusionInvoice.MPermPenalty).
   add_double(lcMobileStruct,"total_to_pay",FusionInvoice.MInvAmt).
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
