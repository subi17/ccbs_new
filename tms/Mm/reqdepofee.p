/* ---------------------------------------------------------------------------
  MODULE .......: REQDEPOFEE
  FUNCTION .....: Create deposit single fee and a deposit invoice
  APPLICATION ..: TMS
  CREATED ......: 24.02.06/aam 
  MODIFIED .....: 
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/eventval.i}
{Func/timestamp.i}

DEF INPUT  PARAMETER iiRequest AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER olCreated AS LOG  NO-UNDO. 
DEF OUTPUT PARAMETER ocError   AS CHAR NO-UNDO. 

DEF VAR ldAmount      AS DEC  NO-UNDO. 
DEF VAR lcDepoItem    AS CHAR NO-UNDO. 
DEF VAR liDepoCust    AS INT  NO-UNDO. 
DEF VAR liBillPeriod  AS INT  NO-UNDO. 
DEF VAR liConcerns    AS INT  NO-UNDO. 
DEF VAR liQty         AS INT  NO-UNDO. 
DEF VAR lcClass       AS CHAR NO-UNDO.  
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

FUNCTION fReqValues RETURNS LOGICAL:

   ASSIGN Customer.CustName   = ENTRY(1,MsRequest.ReqCParam1,";")
          Customer.FirstName  = ENTRY(2,MsRequest.ReqCParam1,";")
          Customer.COName     = ENTRY(3,MsRequest.ReqCParam1,";")
          Customer.Address    = ENTRY(4,MsRequest.ReqCParam1,";")
          Customer.ZipCode    = ENTRY(5,MsRequest.ReqCParam1,";")
          Customer.PostOffice = ENTRY(6,MsRequest.ReqCParam1,";")
          Customer.Country    = ENTRY(7,MsRequest.ReqCParam1,";").
          
   IF ENTRY(8,MsRequest.ReqCParam1,";") > "" THEN 
      Customer.EMail = ENTRY(8,MsRequest.ReqCParam1,";").
      
   IF ENTRY(9,MsRequest.ReqCParam1,";") > "" THEN 
      Customer.SMSNumber = ENTRY(9,MsRequest.ReqCParam1,";").
 
END FUNCTION.


olCreated = FALSE.
        
FIND MsRequest EXCLUSIVE-LOCK WHERE 
     MsRequest.MsRequest = iiRequest NO-ERROR.
IF NOT AVAILABLE MsRequest THEN DO:
   ocError = "Unknown request".
   RETURN.
END.

/* create customer if an old one was not used */
IF MsRequest.ReqIParam1 = 0 THEN DO:
       
   /* is there already a customer with given personid */
   IF CAN-FIND(FIRST Customer WHERE
                     Customer.Brand = gcBrand AND
                     Customer.OrgID = ENTRY(11,MsRequest.ReqCParam1,";"))
   THEN DO: 
      ocError = "There is already a customer with given person ID. " +
                "It should be selected to this request before " + 
                "creating an invoice.".
      RETURN.
   END. 

   RUN pCreateCustomer(MsRequest.MsSeq,
                       OUTPUT MsRequest.ReqIParam1).
                             
   IF MsRequest.ReqIParam1 = 0 THEN DO:
      ocError = "New customer could not be created, invoice can " +
                "not be created.".
      RETURN.  
   END. 
END.

ELSE DO:

   /* if vrk has been run succesfully for old owner then update 
      old owner's data */
   IF MsRequest.ReqDParam1 = 1 THEN DO:
      FIND Customer WHERE Customer.CustNum = MsRequest.ReqIParam1 
         EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN DO:
         fReqValues().
      END. 
   END.
END.
 
/* invoice type to be created */
liInvType = fCParamI("InvCreType").
IF liInvType = 0 OR liInvType = ? THEN liInvType = 4.

/* deposit invoice */
IF liInvType = 3 THEN ASSIGN 
   /* billing item for fee */
   lcDepoItem    = fCParamC("DepositItem")
   /* amount according to given credit class */
   ldAmount      = fCParamDE("DepoFeeAmt" + lcClass)
   /* customer to which deposit invoice is created */
   liDepoCust    = fCParamI("DepositCust")
   /* letterclass for epl */
   liLetterClass = fCParamI("EPLDepoLClass"). 

/* adv.payment invoice */
ELSE IF liInvType = 4 THEN ASSIGN 
   /* billing item for fee */
   lcDepoItem    = fCParamC("AdvPaymItem")
   /* amount according to given credit class */
   ldAmount      = fCParamDE("AdvPaymAmt" + lcClass)
   /* customer to which invoice is created */
   liDepoCust    = fCParamI("AdvPaymCust")
   /* letterclass for epl */
   liLetterClass = fCParamI("EPLAdvPaymLClass"). 

ELSE DO:
   ocError = "Unknown invoice type " + STRING(liInvType).
   RETURN.
END. 

lcEPLFile = fCParamC("EPLFile").


IF ldAmount = 0 OR ldAmount = ? THEN DO:
   ocError = "Amount for class " + lcClass + " has not been defined".
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
     Customer.CustNum = MsRequest.ReqIParam1 NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   ocError = "Unknown customer " + STRING(MsRequest.ReqIParam1).
   RETURN. 
END.

ASSIGN liBillPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
       liConcerns   = liBillPeriod * 100 + DAY(TODAY). 

/* already done (should invoice creation be tried if billed = false ?) */
FOR FIRST SingleFee NO-LOCK WHERE
          SingleFee.Brand     = gcBrand     AND
          SingleFee.HostTable = "MsRequest" AND
          SingleFee.KeyValue  = STRING(MsRequest.MsRequest) AND
          SingleFee.BillCode  = lcDepoItem:
   ocError = "Fee already exists".
   RETURN.
END.

/* make deposit single fee */
DO FOR SingleFee:
   
   CREATE SingleFee.

   ASSIGN
   SingleFee.Brand       = gcBrand 
   SingleFee.FMItemId    = NEXT-VALUE(bi-seq)
   SingleFee.CustNum     = liDepoCust    
   SingleFee.BillTarget  = 1
   SingleFee.CalcObj     = lcClass
   SingleFee.BillCode    = lcDepoItem       
   SingleFee.BillPeriod  = liBillPeriod     /* billing Period   */
   SingleFee.Concerns[1] = liConcerns       /* period concerned */
   SingleFee.Amt         = ldAmount         /* Payment          */
   SingleFee.Memo[1]     = BillItem.BIName
   SingleFee.Memo[2]     = ""
   SingleFee.HostTable   = "MsRequest"
   SingleFee.KeyValue    = STRING(MsRequest.MsRequest)
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
RUN Inv/nnlamu5 (MsRequest.ReqIParam1,
             MsRequest.MsRequest,
             "",
             liInvType,
             TRUE,
             OUTPUT liQty).

IF liQty = 0 THEN ocError = "Invoice was not created".

/* make an EPL file */
ELSE DO:

   ASSIGN liQty     = 0
          olCreated = TRUE.
   
   FOR FIRST SingleFee NO-LOCK WHERE
             SingleFee.Brand    = gcBrand AND
             SingleFee.FMItemId = liSingle,
       FIRST Invoice NO-LOCK WHERE
             Invoice.InvNum = SingleFee.InvNum:
             
       RUN Inv/eletterinv(INPUT Invoice.InvNum,
                     INPUT Invoice.InvNum,
                     INPUT Invoice.InvDate,
                     INPUT "",
                     INPUT Invoice.CustNum,
                     INPUT FALSE,
                     INPUT TRUE,
                     INPUT TRUE,
                     INPUT Invoice.InvType,
                     INPUT liLetterClass,
                     INPUT lcEPLFile,
                     OUTPUT liQty,
                     OUTPUT ocError).
   END.
      
   IF liQty = 0 then ocError = "Printing failed; " + ocError.                   
END.

PROCEDURE pCreateCustomer:

   DEF INPUT PARAMETER iiMsSeq    AS INT NO-UNDO.
   DEF OUTPUT PARAMETER oiNewCust AS INT NO-UNDO.
   
   DEF VAR liDefCust AS INT  NO-UNDO. 

   FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN DO:
      MESSAGE "Subscription was not found"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   /* use template from current customer's group */
   FIND Customer WHERE Customer.CustNum = MobSub.AgrCust NO-LOCK. 
   
   liDefCust = fCParamI("DefCust" + Customer.InvGroup + "/" + 
                        ENTRY(10,MsRequest.ReqCParam1,";")).
      
   IF liDefCust = ? OR liDefCust = 0 THEN DO:
      MESSAGE "Default customer not defined for " + Customer.InvGroup.
      RETURN.
   END. 
      
   RUN Mm/copymobcu (INPUT-OUTPUT liDefCust,
                  FALSE).
      
   FIND Customer WHERE Customer.CustNum = liDefCust EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE Customer THEN DO:
      
      ASSIGN Customer.ChgStamp   = fMakeTS()
             Customer.CreUser    = katun
             Customer.InvCust    = Customer.CustNum
             Customer.PaymCust   = Customer.CustNum
             Customer.RepCust    = Customer.CustNum
             Customer.RateCust   = Customer.CustNum 
             Customer.AgrCust    = Customer.CustNum
             Customer.Category   = ENTRY(10,MsRequest.ReqCParam1,";")
             Customer.OrgID      = ENTRY(11,MsRequest.ReqCParam1,";")
             Customer.ContrBeg   = TODAY
             Customer.SearchName = SUBSTRING(Customer.CustName + " " + 
                                             Customer.FirstName,1,8)
             oiNewCust           = Customer.CustNum.

      fReqValues().
      
      FIND CustCat WHERE 
           CustCat.Brand    = gcBrand AND
           CustCat.Category = Customer.Category 
      NO-LOCK NO-ERROR.
      IF AVAILABLE CustCat THEN Customer.PaymTerm = CustCat.PaymTerm.
   END.

END PROCEDURE.


