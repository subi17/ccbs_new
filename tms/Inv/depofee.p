/* ---------------------------------------------------------------------------
  MODULE .......: DEPOFEE
  FUNCTION .....: Create deposit single fee and a deposit invoice
  APPLICATION ..: TMS
  CREATED ......: 19.01.04/aam 
  MODIFIED .....: 23.04.04/aam invoice type from InvCreType
                  23.08.04/aam orderid to nnlamu5
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/eventval.i}

DEF INPUT  PARAMETER iiOrder  AS INT  NO-UNDO. 
DEF INPUT  PARAMETER icClass  AS CHAR NO-UNDO.  
DEF INPUT  PARAMETER idAmt    AS DEC  NO-UNDO. 
DEF OUTPUT PARAMETER ocError  AS CHAR NO-UNDO. 

DEF VAR ldAmount     AS DEC  NO-UNDO. 
DEF VAR lcDepoItem   AS CHAR NO-UNDO. 
DEF VAR liDepoCust   AS INT  NO-UNDO. 
DEF VAR liBillPeriod AS INT  NO-UNDO. 
DEF VAR liConcerns   AS INT  NO-UNDO. 
DEF VAR liQty        AS INT  NO-UNDO. 

DEF VAR liSingle      AS INT  NO-UNDO. 
DEF VAR lcEPLFile     AS CHAR NO-UNDO.
DEF VAR liLetterClass AS INT  NO-UNDO. 
DEF VAR liInvType     AS INT  NO-UNDO. 


IF llDoEvent THEN DO FOR SingleFee:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   RUN StarEventInitialize(lhSingleFee).
              
END.

        
FIND Order NO-LOCK WHERE 
     Order.Brand   = gcBrand AND
     Order.OrderID = iiOrder NO-ERROR.
IF NOT AVAILABLE Order THEN DO:
   ocError = "Unknown order".
   RETURN.
END.

/* invoice type to be created */
liInvType = fCParamI("InvCreType").
IF liInvType = 0 OR liInvType = ? THEN liInvType = 3.

/* deposit invoice */
IF liInvType = 3 THEN ASSIGN 
   /* billing item for fee */
   lcDepoItem    = fCParamC("DepositItem")
   /* amount according to given credit class */
   ldAmount      = fCParamDE("DepoFeeAmt" + icClass)
   /* customer to which deposit invoice is created */
   liDepoCust    = fCParamI("DepositCust")
   /* letterclass for epl */
   liLetterClass = fCParamI("EPLDepoLClass"). 

/* adv.payment invoice */
ELSE IF liInvType = 4 THEN ASSIGN 
   /* billing item for fee */
   lcDepoItem    = fCParamC("AdvPaymItem")
   /* amount according to given credit class */
   ldAmount      = fCParamDE("AdvPaymAmt" + icClass)
   /* customer to which invoice is created */
   liDepoCust    = fCParamI("AdvPaymCust")
   /* letterclass for epl */
   liLetterClass = fCParamI("EPLAdvPaymLClass"). 

ELSE DO:
   ocError = "Unknown invoice type " + STRING(liInvType).
   RETURN.
END. 

lcEPLFile = fCParamC("EPLFile").

/* amount has been given */
IF idAmt NE 0 THEN ldAmount = idAmt.

IF ldAmount = 0 OR ldAmount = ? THEN DO:
   ocError = "Amount for class " + icClass + " has not been defined".
   RETURN.
END.

FIND BillItem NO-LOCK WHERE     
     BillItem.Brand    = gcBrand AND
     BillItem.BillCode = lcDepoItem NO-ERROR.
IF NOT AVAILABLE BillItem THEN DO:
   ocError = "Unknown billing item " + lcDepoItem.
   RETURN.
END. 
   
FIND Customer NO-LOCK WHERE
     Customer.Brand   = gcBrand AND
     Customer.CustNum = liDepoCust NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   ocError = "Unknown deposit/adv.paym customer " + STRING(liDepoCust).
   RETURN. 
END.

ASSIGN liBillPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
       liConcerns   = liBillPeriod * 100 + DAY(TODAY). 

/* already done (should invoice creation be tried if billed = false ?) */
FOR FIRST SingleFee NO-LOCK WHERE
          SingleFee.Brand     = gcBrand AND
          SingleFee.HostTable = "Order" AND
          SingleFee.KeyValue  = STRING(Order.OrderID) AND
          SingleFee.BillCode  = lcDepoItem:
   ocError = "Fee already exists".
   RETURN.
END.

/* make deposit single fee */
DO FOR SingleFee TRANS:
   
   CREATE SingleFee.

   ASSIGN
   SingleFee.Brand       = gcBrand 
   SingleFee.FMItemId    = NEXT-VALUE(bi-seq)
   SingleFee.CustNum     = liDepoCust    
   SingleFee.BillTarget  = 1
   SingleFee.CalcObj     = icClass
   SingleFee.BillCode    = lcDepoItem       
   SingleFee.BillPeriod  = liBillPeriod     /* billing Period   */
   SingleFee.Concerns[1] = liConcerns       /* period concerned */
   SingleFee.Amt         = ldAmount         /* Payment          */
   SingleFee.Memo[1]     = BillItem.BIName
   SingleFee.Memo[2]     = ""
   SingleFee.HostTable   = "Order"
   SingleFee.KeyValue    = STRING(Order.OrderID)
   SingleFee.BillType    = "SF"
   SingleFee.Contract    = ""
   SingleFee.Active      = TRUE
   SingleFee.FeeModel    = ""
   SingleFee.VATIncl     = Customer.VatIncl
   liSingle              = SingleFee.FMItemId.

   IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSingleFee).

   RELEASE SingleFee.
   
END.

/* create invoice */
RUN Inv/nnlamu5 (liDepoCust,
             Order.OrderID,
             "",
             liInvType,
             TRUE,
             OUTPUT liQty).

IF liQty = 0 THEN ocError = "Invoice was not created".

