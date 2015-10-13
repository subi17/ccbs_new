{commpaa.i}
gcbrand = "1".
katun = "Qvantel".
{tmsconst.i}
{timestamp.i}
{fmakemsreq.i}
{femailinvoice.i}

DEF VAR liRequest AS INT  NO-UNDO.
DEF VAR lcResult  AS CHAR NO-UNDO.

DEF TEMP-TABLE ttFusion NO-UNDO
    FIELD CustNum  AS INT
    FIELD MsSeq    AS INT
    FIELD BundleId AS CHAR.

FOR EACH ServiceLimit WHERE
         LOOKUP(ServiceLimit.GroupCode,"CONTFF2,CONTSF10,CONTSF14") > 0 NO-LOCK,
    EACH MServiceLimit WHERE
         MServiceLimit.SlSeq = ServiceLimit.SlSeq AND
         MServiceLimit.DialType = 7 AND
         MServiceLimit.EndTS >= 20130901 NO-LOCK,
   FIRST MsOwner WHERE
         MsOwner.MsSeq  = MServiceLimit.MsSeq AND
         MsOwner.TSEnd >= MServiceLimit.FromTS NO-LOCK,
   FIRST MobSub WHERE
         MobSub.MsSeq = MsOwner.MsSeq NO-LOCK:

   IF NOT CAN-FIND(FIRST ttFusion WHERE
                         ttFusion.CustNum = MsOwner.CustNum NO-LOCK)
   THEN DO:
      CREATE ttFusion.
      ASSIGN ttFusion.CustNum = MsOwner.CustNum
             ttFusion.MsSeq = MsOwner.MsSeq
             ttFusion.BundleId = ServiceLimit.GroupCode.
   END.
   status default string(MsOwner.MsSeq).
END.

Output to "/apps/yoigo/tms_support/testing/send_fusion_validation_email_1.txt".

FOR EACH ttFusion NO-LOCK:

   FIND FIRST Customer WHERE
              Customer.Custnum = ttFusion.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN DO:
      PUT UNFORMATTED STRING(ttFusion.CustNum) "|ERROR:Customer not found" skip.
      NEXT.
   END.

   FIND FIRST InvoiceTargetGroup WHERE
              InvoiceTargetGroup.CustNum = Customer.CustNum AND
              InvoiceTargetGroup.ToDate >= TODAY AND
             (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
              InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
        EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL InvoiceTargetGroup THEN DO:
      PUT UNFORMATTED STRING(ttFusion.CustNum) "|ERROR:InvoiceTargetGroup not found" skip.
      NEXT.
   END.

   /*IF InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL} THEN DO:
      IF Customer.Email = "" THEN
         PUT UNFORMATTED STRING(ttFusion.CustNum) "|ERROR_EMAIL:InvoiceTargetGroup is Email but email address is missing" skip.
      ELSE IF Customer.DelType = 2 THEN
         PUT UNFORMATTED STRING(ttFusion.CustNum) "|NO_CHANGE:InvoiceTargetGroup and deltype is email" "|CustDelType: " STRING(Customer.DelType) skip.
      ELSE IF Customer.DelType = 11 THEN DO:
         InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}.
         PUT UNFORMATTED STRING(ttFusion.CustNum) "|UPDATED|InvoiceTargetGroup is updated from Fusion Email to Pending Fusion Email as customer deltype is pending email CustDelType: " STRING(Customer.DelType) SKIP.
      END.
      ELSE DO:
         liRequest = fEmailInvoiceRequest(INPUT fMakeTS(),
                                          INPUT TODAY,
                                          INPUT katun,
                                          INPUT 0, /* msseq */
                                          INPUT "", /* cli */
                                          INPUT Customer.CustNum,
                                          INPUT "30",
                                          INPUT Customer.Email,
                                          OUTPUT lcResult).
         IF liRequest > 0 THEN DO:
            InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}.
            PUT UNFORMATTED STRING(ttFusion.CustNum) "|EMAIL_SENT|InvoiceTargetGroup already Email but deltype is other than email CustDelType: " STRING(Customer.DelType) SKIP.
         END.
         ELSE
            PUT UNFORMATTED STRING(ttFusion.CustNum) "|EMAIL_NOT_SENT|InvoiceTargetGroup already Email but deltype is other than email CustDelType: " STRING(Customer.DelType) ":" lcResult SKIP.
      END.
      NEXT.
   END.*/

   IF InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} THEN DO:
      IF Customer.Email = "" THEN
         PUT UNFORMATTED STRING(ttFusion.CustNum) "|ERROR_EMAIL:InvoiceTargetGroup is pending Email but email address is missing" skip.
      ELSE IF Customer.DelType = 11 THEN
         PUT UNFORMATTED STRING(ttFusion.CustNum) "|NO_CHANGE:InvoiceTargetGroup and deltype is pending email" "|CustDelType: " STRING(Customer.DelType) skip.
      ELSE IF Customer.DelType = 2 THEN DO:
         InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL}.
         PUT UNFORMATTED STRING(ttFusion.CustNum) "|UPDATED|InvoiceTargetGroup is updated from Fusion pending Email to Fusion Email and deltype is email CustDelType: " STRING(Customer.DelType) SKIP.
      END.
      ELSE DO:
         liRequest = fEmailInvoiceRequest(INPUT fMakeTS(),
                                          INPUT TODAY,
                                          INPUT katun,
                                          INPUT 0, /* msseq */
                                          INPUT "", /* cli */
                                          INPUT Customer.CustNum,
                                          INPUT "30",
                                          INPUT Customer.Email,
                                          OUTPUT lcResult).
         IF liRequest = 1 THEN DO:
            InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL}.
            PUT UNFORMATTED STRING(ttFusion.CustNum) "|UPDATED|InvoiceTargetGroup is updated from Fusion pending Email to Fusion Email because customer already verified but deltype is other than email CustDelType: " STRING(Customer.DelType) SKIP.
         END.
         ELSE IF liRequest > 0 THEN
            PUT UNFORMATTED STRING(ttFusion.CustNum) "|EMAIL_SENT|InvoiceTargetGroup already Pending Email but deltype is other than email CustDelType: " STRING(Customer.DelType) SKIP.
         ELSE
            PUT UNFORMATTED STRING(ttFusion.CustNum) "|EMAIL_NOT_SENT|InvoiceTargetGroup is pending Email but deltype is other than email CustDelType: " STRING(Customer.DelType) " " lcResult SKIP.
      END.
      NEXT.
   END.
END.