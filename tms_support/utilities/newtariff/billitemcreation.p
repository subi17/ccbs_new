/*------------------------------------------------------------------------
  MODULE .......: billitemcreation.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Fri Feb 06 13:40:29 EET 2015
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/
  
/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER icBaseFile AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icFile     AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icSpoolDir AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttBillItem NO-UNDO
   FIELD BillItem   AS CHARACTER 
   FIELD BIName     AS CHARACTER 
   FIELD BIGroup    AS CHARACTER 
   FIELD AccNum     AS CHARACTER 
   FIELD InvSect    AS CHARACTER 
   FIELD TaxClass   AS CHARACTER 
   FIELD SAPRid     AS CHARACTER 
   FIELD ItemType   AS INT
   FIELD CostCenter AS CHARACTER. 
 
DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE llFirst AS LOGICAL INITIAL TRUE NO-UNDO.

DEFINE STREAM BIIn.
DEFINE STREAM BILog.
/* ********************  Functions  ******************** */

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   icMessage = "ERROR:" + icMessage.
   
   PUT STREAM BILog UNFORMATTED
      TODAY                   " | " 
      STRING(TIME,"HH:MM:SS") " | "
      icMessage SKIP.
      
END FUNCTION.

/* ***************************  Main Block  *************************** */

lcLogFile   = icSpoolDir + icBaseFile + ".log".

INPUT STREAM BIIn FROM VALUE(icFile).
OUTPUT STREAM BILog TO VALUE(lcLogFile) APPEND.
                          
REPEAT:

   IMPORT STREAM BIIn UNFORMATTED lcLine.
   
   /* Ignore the first line - (Header) */
   IF llFirst
   THEN DO:
      llFirst = FALSE.
      NEXT.
   END.
    
   CREATE ttBillItem.
   ASSIGN ttBillItem.BillItem   = TRIM(ENTRY(1,lcLine,";"))
          ttBillItem.BIName     = TRIM(ENTRY(2,lcLine,";")) 
          ttBillItem.BIGroup    = TRIM(ENTRY(3,lcLine,";"))
          ttBillItem.AccNum     = TRIM(ENTRY(4,lcLine,";"))
          ttBillItem.InvSect    = TRIM(ENTRY(5,lcLine,";"))
          ttBillItem.TaxClass   = TRIM(ENTRY(6,lcLine,";"))
          ttBillItem.SAPRid     = TRIM(ENTRY(7,lcLine,";"))
          ttBillItem.CostCenter = TRIM(ENTRY(8,lcLine,";"))
          ttBillItem.ItemType   = INT(ENTRY(9,lcLine,";")) 
          WHEN NUM-ENTRIES(lcLine,";") > 8 NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Incorrect input data").
      RETURN "ERROR". 
   END.
   
END.

RUN pValidateFileData.

IF RETURN-VALUE <> "OK" THEN 
   RETURN RETURN-VALUE. 

RUN pCreateBillingItem.

IF RETURN-VALUE <> "OK" THEN 
   RETURN RETURN-VALUE.   

OUTPUT STREAM BILog CLOSE.
INPUT  STREAM BIIn  CLOSE.

RETURN "OK".

/* ***************************  Main End  *************************** */

   
PROCEDURE pValidateFileData:
   
   BILL-ITEM:
   FOR EACH ttBillItem NO-LOCK:
      IF ttBillItem.BillItem EQ "" THEN DO:
         fError("No Billing Item code data available").
         RETURN "ERROR".
      END. 
      
      IF ttBillItem.BIName EQ "" THEN DO:
         fError("No Billing Item name data available").
         RETURN "ERROR".
      END.
      
      IF ttBillItem.BIGroup EQ "" THEN DO:
         fError("No Billing Item group data available").
         RETURN "ERROR".
      END.
      
      IF ttBillItem.AccNum EQ "" THEN DO:
         fError("No Posting Account data available").
         RETURN "ERROR".
      END. 
      
      IF ttBillItem.TaxClass EQ "" THEN DO:                  
         fError("No Tax class data available").
         RETURN "ERROR".
      END.
      
      IF ttBillItem.CostCenter EQ "" THEN DO: 
         fError("No Cost center data available").
         RETURN "ERROR".
      END.
   END.        
       
   RETURN "OK".
      
END PROCEDURE.    

PROCEDURE pCreateBillingItem:

   FOR EACH ttBillItem NO-LOCK:
      FIND BillItem WHERE 
           BillItem.BillCode = ttBillItem.BillItem 
      NO-LOCK NO-ERROR.

      IF AVAILABLE BillItem THEN DO: 
         fError(SUBSTITUTE("Billing Item '&1' already exists", ttBillItem.BillItem)).
         RETURN "ERROR". 
      END.
      
      FIND BItemGroup WHERE  
           BItemGroup.Brand   = Syst.Var:gcBrand AND
           BItemGroup.BIGroup = ttBillItem.BIGroup  
      NO-LOCK NO-ERROR.  
            
      IF NOT AVAILABLE BItemGroup THEN DO:
         fError(SUBSTITUTE("Unknown billing item group '&1'", ttBillItem.BIGroup)).
         RETURN "ERROR". 
      END.
      
      FIND Account WHERE  
           Account.Brand  = Syst.Var:gcBrand AND
           Account.AccNum = INTEGER(ttBillItem.AccNum) 
      NO-LOCK NO-ERROR.
                        
      IF NOT AVAILABLE Account THEN DO:
         fError(SUBSTITUTE("Unknown account number '&1'",ttBillItem.AccNum)).
         RETURN "ERROR". 
      END.
      
      FIND TaxClass WHERE
           TaxClass.TaxClass = ttBillItem.TaxClass NO-LOCK NO-ERROR.
                 
      IF NOT AVAILABLE TaxClass THEN DO:
         fError(SUBSTITUTE("Unknown tax class '&1'",ttBillItem.TaxClass)).
         RETURN "ERROR".
      END.      

      IF ttBillItem.InvSect > ""
      THEN DO:
         FIND InvSect WHERE 
            InvSect.Brand   = Syst.Var:gcBrand AND
            InvSect.InvSect = ttBillItem.InvSect
         NO-LOCK NO-ERROR.
         IF NOT AVAIL InvSect THEN DO:
            fError(SUBSTITUTE("InvSect have invalid value '&1'", ttBillItem.InvSect)).
            RETURN "ERROR".
         END.
      END.

      BILLIING-ITEM:
      DO TRANSACTION ON ERROR UNDO,LEAVE:
      
          CREATE BillItem.
          ASSIGN BillItem.Brand       = Syst.Var:gcBrand
                 BillItem.DispMPM     = FALSE
                 BillItem.BillCode    = ttBillItem.BillItem
                 Billitem.BIName      = ttBillItem.BIName
                 BillItem.BIGroup     = ttBillItem.BIGroup
                 BillItem.InvSect     = ttBillItem.InvSect
                 BillItem.TaxClass    = ttBillItem.TaxClass
                 BillItem.CostCentre  = ttBillItem.CostCenter
                 BillItem.ItemType    = ttBillItem.ItemType NO-ERROR.
                 
          IF ERROR-STATUS:ERROR THEN DO:               
              fError("Error in creating BillingItem").
              RETURN "ERROR".        
          END.
          
          CREATE CCRule.
          ASSIGN 
              CCRule.Brand       = BillItem.Brand
              CCRule.CCRuleID    = NEXT-VALUE(CCRuleSeq)
              CCRule.Category    = "*"
              CCRule.BillCode    = BillItem.BillCode
              CCRule.ValidFrom   = TODAY
              CCRule.ValidTo     = DATE(12,31,2049) 
              CCRule.AccNum      = INTEGER(ttBillItem.AccNum)
              CCRule.EUAccNum    = INTEGER(ttBillItem.AccNum)
              CCRule.EUConAccNum = INTEGER(ttBillItem.AccNum)
              CCRule.FSAccNum    = INTEGER(ttBillItem.AccNum)
              CCRule.CostCentre  = BillItem.CostCentre
              CCRule.ReportingID = ttBillItem.SAPRid NO-ERROR.    
              
          IF ERROR-STATUS:ERROR THEN 
          DO:              
              fError("Error in creating Accounting Rule. BillingItem record not created.").
              RETURN "ERROR".
          END.
      END.       

   END.    

   RETURN "OK".        
        
END PROCEDURE.