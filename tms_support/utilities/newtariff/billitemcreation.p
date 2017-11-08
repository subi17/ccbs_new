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
   FIELD PostAcct   AS CHARACTER 
   FIELD TaxClass   AS CHARACTER 
   FIELD IFSCode    AS CHARACTER 
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
          ttBillItem.PostAcct   = TRIM(ENTRY(4,lcLine,";"))
          ttBillItem.TaxClass   = TRIM(ENTRY(5,lcLine,";"))
          ttBillItem.IFSCode    = TRIM(ENTRY(6,lcLine,";"))
          ttBillItem.CostCenter = TRIM(ENTRY(7,lcLine,";"))
          ttBillItem.ItemType   = INT(ENTRY(8,lcLine,";")) 
          WHEN NUM-ENTRIES(lcLine,";") > 7 NO-ERROR.
   
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
      
      IF ttBillItem.PostAcct EQ "" THEN DO:
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
         fError("Billing Item already exists").
         RETURN "ERROR". 
      END.
      
      FIND BItemGroup WHERE  
           BItemGroup.Brand   = Syst.Var:gcBrand AND
           BItemGroup.BIGroup = ttBillItem.BIGroup  
      NO-LOCK NO-ERROR.  
            
      IF NOT AVAILABLE BItemGroup THEN DO:
         fError("Wrong Billing Item group data").
         RETURN "ERROR". 
      END.
      
      FIND Account WHERE  
           Account.Brand  = Syst.Var:gcBrand AND
           Account.AccNum = INTEGER(ttBillItem.PostAcct) 
      NO-LOCK NO-ERROR.
                        
      IF NOT AVAILABLE Account THEN DO:
         fError("Wrong Account number data").
         RETURN "ERROR". 
      END.
      
      FIND TaxClass WHERE
           TaxClass.TaxClass = ttBillItem.TaxClass NO-LOCK NO-ERROR.
                 
      IF NOT AVAILABLE TaxClass THEN DO:
         fError("Wrong Tax class data available").
         RETURN "ERROR".
      END.      
      
      CREATE BillItem.
      ASSIGN BillItem.Brand       = Syst.Var:gcBrand
             BillItem.DispMPM     = FALSE
             BillItem.BillCode    = ttBillItem.BillItem
             Billitem.BIName      = ttBillItem.BIName
             BillItem.BIGroup     = ttBillItem.BIGroup
             BillItem.AccNum      = INTEGER(ttBillItem.PostAcct) 
             BillItem.AltAccNum   = INTEGER(ttBillItem.PostAcct)
             BillItem.VipAccNum   = INTEGER(ttBillItem.PostAcct) 
             BillItem.EUConAccNum = INTEGER(ttBillItem.PostAcct)
             BillItem.EUAccNum    = INTEGER(ttBillItem.PostAcct)
             BillItem.FSAccNum    = INTEGER(ttBillItem.PostAcct)
             BillItem.TaxClass    = ttBillItem.TaxClass
             BillItem.SAPRid      = ttBillItem.IFSCode
             BillItem.CostCentre  = ttBillItem.CostCenter
             BillItem.ItemType    = ttBillItem.ItemType NO-ERROR.
             
      IF ERROR-STATUS:ERROR THEN DO: 
         fError("Error in creating BillingItem").
         RETURN "ERROR".        
      END.   

   END.    

   RETURN "OK".        
        
END PROCEDURE.