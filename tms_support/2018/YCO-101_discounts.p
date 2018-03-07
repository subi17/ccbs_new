DEFINE VARIABLE giDPId AS INTEGER NO-UNDO.

FUNCTION fGetDiscountPlan RETURNS INTEGER
   (icDPRuleID AS CHARACTER ):

   FIND FIRST DiscountPlan NO-LOCK WHERE
              DiscountPlan.Brand    = Syst.Var:gcBrand AND
              DiscountPlan.DPRuleID = icDPRuleID
   NO-ERROR.
              
   IF AVAILABLE DiscountPlan
   THEN RETURN DiscountPlan.DPId.

END FUNCTION.

FUNCTION fCreateDPSubject RETURNS LOGICAL
   ( iiDPId AS INTEGER,
     icDPSubject AS CHARACTER ):

   FIND FIRST DPSubject NO-LOCK WHERE
              DPSubject.DPId = iiDPId AND
              DPSubject.DPSubject = icDPSubject
   NO-ERROR.

   IF NOT AVAILABLE DPSubject
   THEN DO:              
      CREATE DPSubject.
      ASSIGN
         DPSubject.DPId       = iiDPId                
         DPSubject.DPSubject  = icDPSubject                 
         DPSubject.ValidFrom  = 2/26/2018                  
         DPSubject.ValidTo    = 12/31/2049.
   END.

   RETURN TRUE.

END FUNCTION.

FUNCTION fCreateDPTarget RETURNS LOGICAL
   ( iiDPId AS INTEGER,
     icTargetKey AS CHARACTER ):

   FIND FIRST DPTarget NO-LOCK WHERE
              DPTarget.DPId = iiDPId AND
              DPTarget.TargetTable = "BillItem" AND
              DPTarget.TargetKey = icTargetKey
   NO-ERROR.

   IF NOT AVAILABLE DPTarget
   THEN DO:
      CREATE DPTarget.
      ASSIGN
         DPTarget.DPId        = iiDPId
         DPTarget.ValidFrom   = 2/26/2018
         DPTarget.ValidTo     = 12/31/2049
         DPTarget.TargetTable = "BillItem"
         DPTarget.TargetKey   = icTargetKey
         DPTarget.Included    = YES.
   END.

   RETURN TRUE.

END FUNCTION.

giDPId = fGetDiscountPlan("ASISTDISC").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTDSL2G").
   fCreateDPSubject(giDPId, "CONTFH2G_50").
   fCreateDPSubject(giDPId, "CONTFH2G_300").
   fCreateDPSubject(giDPId, "CONTFH2G_1000").
END.

giDPId = fGetDiscountPlan("DISCFH300").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTFH2G_300").
END.

giDPId = fGetDiscountPlan("DISCFH300P").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTFH2G_300").
END.

giDPId = fGetDiscountPlan("DISCWINBACK").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTDSL2G").
END.

/* BONO7DISC is enabled for every CLIType having BONO7 support, including
   CONTDSL2G and CONTFH2G_1000 which were not included to the specification
   page. */

giDPId = fGetDiscountPlan("FIDETVDISC").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTFH2G_50").
   fCreateDPSubject(giDPId, "CONTFH2G_300").
   fCreateDPSubject(giDPId, "CONTDSL2G"). /* NOT VERIFIED IF THIS IS NEEDED */
   fCreateDPSubject(giDPId, "CONTFH2G_1000"). /* NOT VERIFIED IF THIS IS NEEDED */
END.


giDPId = fGetDiscountPlan("CONVDISC").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTDSL2G"). /* NOT VERIFIED IF THIS IS NEEDED */
   fCreateDPSubject(giDPId, "CONTFH2G_50"). /* NOT VERIFIED IF THIS IS NEEDED */
   fCreateDPSubject(giDPId, "CONTFH2G_300"). /* NOT VERIFIED IF THIS IS NEEDED */
   fCreateDPSubject(giDPId, "CONTFH2G_1000"). /* NOT VERIFIED IF THIS IS NEEDED */
END.

giDPId = fGetDiscountPlan("GMMDISC").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTDSL2G").
   fCreateDPSubject(giDPId, "CONTFH2G_50").
   fCreateDPSubject(giDPId, "CONTFH2G_300").
   fCreateDPSubject(giDPId, "CONTFH2G_1000"). /* NOT VERIFIED IF THIS IS NEEDED */
END.

giDPId = fGetDiscountPlan("CONVDISC_CS100_6").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTDSL2G").
   fCreateDPSubject(giDPId, "CONTFH2G_50").
   fCreateDPSubject(giDPId, "CONTFH2G_300").
   fCreateDPSubject(giDPId, "CONTFH2G_1000").
END.

giDPId = fGetDiscountPlan("CONVDISC_CS20F12").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTDSL2G").
   fCreateDPSubject(giDPId, "CONTFH2G_50").
   fCreateDPSubject(giDPId, "CONTFH2G_300").
   fCreateDPSubject(giDPId, "CONTFH2G_1000").
END.

giDPId = fGetDiscountPlan("CONVDISC_CS20M12").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTDSL2G").
   fCreateDPSubject(giDPId, "CONTFH2G_50").
   fCreateDPSubject(giDPId, "CONTFH2G_300").
   fCreateDPSubject(giDPId, "CONTFH2G_1000").
END.

giDPId = fGetDiscountPlan("DISCFH300P_PRO").
IF giDPId NE 0
THEN DO:
   /* As the tariff is not pro related, this is not enabled */
   /*fCreateDPSubject(giDPId, "CONTFH2G_300").*/
END.

giDPId = fGetDiscountPlan("DISCFH300PDWN").
IF giDPId NE 0
THEN DO:
   fCreateDPSubject(giDPId, "CONTFH2G_300"). /* NOT VERIFIED IF THIS IS NEEDED */
END.
