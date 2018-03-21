{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}

   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhBillItem AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhRepText  AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhInvText  AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhDiscountplan  AS HANDLE NO-UNDO.
   
   lhBillItem = BUFFER BillItem:HANDLE.
   lhRepText  = BUFFER Reptext:HANDLE.
   lhInvText  = BUFFER InvText:HANDLE.
   lhDiscountplan = BUFFER Discountplan:HANDLE.
   RUN StarEventInitialize(lhBillItem).
   RUN StarEventInitialize(lhRepText ).
   RUN StarEventInitialize(lhInvText ).
   RUN StarEventInitialize(lhDiscountplan).

/* ANGELTECHMF */
FIND BillItem WHERE BillItem.Brand   =  "1" AND BillItem.Billcode = "ANGELTECHMF" EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF LOCKED BillItem THEN DO:
    MESSAGE "BillItem 'ANGELTECHMF' is locked. Unable to update Config" 
    VIEW-AS ALERT-BOX.
END. 
IF AVAILABLE BillItem THEN DO:
    RUN StarEventSetOldBuffer(lhBillItem).
    ASSIGN 
        BillItem.BiGroup    = "55"
        BillItem.AccNum     = 553800001
        BillItem.InvSect    = "" 
        BillItem.TaxClass   = '0' 
        BillItem.SAPRid     = "500"
        BillItem.CostCentre = "SL" .
    RUN StarEventMakeModifyEvent(lhBillItem).        
END.

/* ASISTMF */
FIND BillItem WHERE BillItem.Brand   =  "1" AND BillItem.Billcode = "ASISTMF" EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF LOCKED BillItem THEN DO:
    MESSAGE "BillItem 'ASISTMF' is locked. Unable to update Config" 
    VIEW-AS ALERT-BOX.
END. 
IF AVAILABLE BillItem THEN DO:
    RUN StarEventSetOldBuffer(lhBillItem).
    ASSIGN 
        BillItem.BIName     = "Soluciona Negocios"
        BillItem.BiGroup    = "55"
        BillItem.AccNum     = 553800001
        BillItem.InvSect    = "" 
        BillItem.TaxClass   = '0' 
        BillItem.SAPRid     = "500"
        BillItem.CostCentre = "SL" .
    RUN StarEventMakeModifyEvent(lhBillItem).
    FOR EACH Reptext WHERE 
             RepText.Brand = billItem.brand AND 
             RepText.TextType = 1 AND 
             reptext.linkcode = billitem.billcode EXCLUSIVE-LOCK :
        RUN StarEventSetOldBuffer(lhRepText).  
        CASE RepText.Language :
            WHEN 1 THEN ASSIGN reptext.reptext = "Soluciona Negocios prestado por HomeServe" . 
            WHEN 2 THEN ASSIGN reptext.reptext = "Soluciona Negocios prestat per HomeServe" .
            WHEN 3 THEN ASSIGN reptext.reptext = "Soluciona Negocios, Homeserve emana" .
            WHEN 5 THEN ASSIGN reptext.reptext = "Soluciona Negocios provided by HomeServe" .
        END CASE.
        RUN StarEventMakeModifyEvent(lhRepText).
    END.
END.
    
    
/* ANGELTECHDISC */
FIND BillItem WHERE BillItem.Brand   =  "1" AND BillItem.Billcode = "ANGELTECHDISC" EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF LOCKED BillItem THEN DO:
    MESSAGE "BillItem 'ANGELTECHDISC' is locked. Unable to update Config" 
    VIEW-AS ALERT-BOX.
END. 
IF AVAILABLE BillItem THEN DO:
    RUN StarEventSetOldBuffer(lhBillItem).
    ASSIGN 
        BillItem.BiGroup    = "13"
        BillItem.AccNum     = 70518110
        BillItem.InvSect    = "" 
        BillItem.TaxClass   = '1' 
        BillItem.SAPRid     = "49"
        BillItem.CostCentre = "SL" .
    RUN StarEventMakeModifyEvent(lhBillItem).
    FOR EACH Reptext WHERE 
             RepText.Brand = billItem.brand AND 
             RepText.TextType = 1 AND 
             reptext.linkcode = billitem.billcode EXCLUSIVE-LOCK :
        RUN StarEventSetOldBuffer(lhRepText).
        CASE RepText.Language :
            WHEN 1 THEN ASSIGN reptext.reptext = "Descuento Promoción" . 
            WHEN 2 THEN ASSIGN reptext.reptext = "Descompte Promoció " .
            WHEN 3 THEN ASSIGN reptext.reptext = "Promozio-deskontua " .
            WHEN 5 THEN ASSIGN reptext.reptext = "Promotional discount " .
        END CASE.
        RUN StarEventMakeModifyEvent(lhRepText).
    END.

END.
    
/* ASISTDISC */
FIND BillItem WHERE BillItem.Brand   =  "1" AND BillItem.Billcode = "ASISTDISC" EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF LOCKED BillItem THEN DO:
    MESSAGE "BillItem 'ASISTDISC' is locked. Unable to update Config" 
    VIEW-AS ALERT-BOX.
END. 
IF AVAILABLE BillItem THEN DO:
    RUN StarEventSetOldBuffer(lhBillItem).
    ASSIGN 
        BillItem.BIName     = "Soluciona Negocios discount"
        BillItem.BiGroup    = "13"
        BillItem.AccNum     = 70518110
        BillItem.InvSect    = "" 
        BillItem.TaxClass   = '1' 
        BillItem.SAPRid     = "49"
        BillItem.CostCentre = "SL" .
    RUN StarEventMakeModifyEvent(lhBillItem).
    FOR EACH Reptext WHERE 
             RepText.Brand = billItem.brand AND 
             RepText.TextType = 1 AND 
             reptext.linkcode = billitem.billcode EXCLUSIVE-LOCK :
        RUN StarEventSetOldBuffer(lhRepText).
        CASE RepText.Language :
            WHEN 1 THEN ASSIGN reptext.reptext = "Descuento Promoción" . 
            WHEN 2 THEN ASSIGN reptext.reptext = "Descompte Promoció " .
            WHEN 3 THEN ASSIGN reptext.reptext = "Promozio-deskontua " .
            WHEN 5 THEN ASSIGN reptext.reptext = "Promotional discount " .
        END CASE.
        RUN StarEventMakeModifyEvent(lhRepText).
    END.
END.

/* Discounts */

FIND DiscountPlan WHERE DiscountPlan.Brand = Syst.var:gcBrand AND DiscountPlan.DPRuleId = "ANGELTECHDISC" AND 
     DiscountPlan.BillCode = "ANGELTECHDISC" EXCLUSIVE-LOCK NO-ERROR.
IF LOCKED discountplan THEN DO:
    MESSAGE "DiscountPlan 'ANGELTECHDISC' is locked. Unable to update Config" 
    VIEW-AS ALERT-BOX.
END. 
IF AVAILABLE discountplan THEN DO:
    RUN StarEventSetOldBuffer(lhDiscountplan).
    ASSIGN 
        DiscountPlan.ValidFrom = 12/01/2017
        DiscountPlan.ValidTo   = 12/31/2049
        DiscountPlan.DPName    = "Angel technology SVA discount"
        DiscountPlan.Priority  = 1
        DiscountPlan.SubjectType = "LIST"
        DiscountPlan.DPUnit    = "Percentage".
    FIND FIRST DPTarget WHERE DPTarget.DPId = DiscountPlan.DPId AND DPTarget.TargetKey = "ANGELTECHMF" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE DPTarget THEN DO:
        CREATE DPTarget .
        ASSIGN 
            DPTarget.DPId = DiscountPlan.DPId
            DPTarget.Included = YES 
            DPTarget.TargetKey = "ANGELTECHMF" 
            DPTarget.TargetTable = "BillItem"
            DPTarget.ValidFrom = DiscountPlan.ValidFrom
            DPTarget.ValidTo   = DiscountPlan.ValidTo   .
    END. 
    RUN StarEventMakeModifyEvent(lhDiscountplan).
END.


FIND DiscountPlan WHERE DiscountPlan.Brand = Syst.var:gcBrand AND DiscountPlan.DPRuleId = "ASISTDISC" AND 
     DiscountPlan.BillCode = "ASISTDISC" EXCLUSIVE-LOCK NO-ERROR.
IF LOCKED discountplan THEN DO:
    MESSAGE "DiscountPlan 'ASISTDISC' is locked. Unable to update Config" 
    VIEW-AS ALERT-BOX.
END. 
IF AVAILABLE discountplan THEN DO:
    RUN StarEventSetOldBuffer(lhDiscountplan).
    ASSIGN 
        DiscountPlan.ValidFrom = 12/01/2017
        DiscountPlan.ValidTo   = 12/31/2049
        DiscountPlan.DPName    = "Soluciona Negocios SVA discount"
        DiscountPlan.Priority  = 1
        DiscountPlan.SubjectType = "LIST"
        DiscountPlan.DPUnit    = "Percentage".
    RUN StarEventMakeModifyEvent(lhDiscountplan).
    FIND FIRST DPTarget WHERE DPTarget.DPId = DiscountPlan.DPId AND DPTarget.TargetKey = "ASISTMF" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE DPTarget THEN DO:
        CREATE DPTarget .
        ASSIGN 
            DPTarget.DPId = DiscountPlan.DPId
            DPTarget.Included = YES 
            DPTarget.TargetKey = "ASISTMF" 
            DPTarget.TargetTable = "BillItem"
            DPTarget.ValidFrom = DiscountPlan.ValidFrom
            DPTarget.ValidTo   = DiscountPlan.ValidTo   .
    END. 
END.

/* Subject change for SVA_ASIST */   
FOR EACH InvText EXCLUSIVE-LOCK WHERE
         InvText.Brand     = Syst.Var:gcBrand   AND
         InvText.Target    = "EMAIL"            AND
         InvText.KeyValue  BEGINS "SVA_ASIST"   AND
         InvText.FromDate <= TODAY              AND
         InvText.ToDate   >= TODAY              AND
         InvText.Language  = 1:
     RUN StarEventSetOldBuffer(lhInvText).
     ASSIGN invtext.TxtTitle = "SVA_ Soluciona Negocios #STATUS".
     RUN StarEventMakeModifyEvent(lhInvText).
END.
  
/* Create SMS InvText */

DO :
    CREATE InvText.
    ASSIGN 
        InvText.Brand     = Syst.Var:gcBrand   
        InvText.ITNum     = NEXT-VALUE(it-seq)
        InvText.Target    = "SMS"            
        InvText.KeyValue  = "CategoryChangeSMS"   
        InvText.FromDate  = TODAY - 1  
        InvText.Todate    = 12/31/49               
        InvText.Language  = 1
        InvText.TemplateID  = ""
        InvText.ParamKeyValue = ""
        InvText.InvText   = "Yoigo info: ¡Que bien! ya tenemos tu solicitud para ser de Yoigo con Oferta Negocios. Necesitamos que nos envíes a Soporte.negocios@yoigo.com la documentación - recibo de autónomos- para seguir con el proceso."
        InvText.UseMMan   = TRUE 
        INvText.category  = "CustomerCategoryChange"
        InvText.TxtTitle  = "Customer Category Change". 

    CREATE InvText.
    ASSIGN 
        InvText.Brand     = Syst.Var:gcBrand
        InvText.ITNum     = NEXT-VALUE(it-seq)   
        InvText.Target    = "EMAIL"            
        InvText.KeyValue  = "SVA_ActEmail"   
        InvText.FromDate  = TODAY - 1  
        InvText.Todate    = 12/31/49               
        InvText.Language  = 1
        InvText.TemplateID  = "SVA_ActEmail_MM"
        InvText.InvText   = ""
        InvText.UseMMan   = TRUE 
        INvText.category  = "SVA Activation"
        InvText.TxtTitle  = "Servicio de valor añadido X activado". 
        
END.

  

