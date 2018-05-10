/* --------------------------------------------------------------------------
  MODULE .......: accdata.i
  FUNCTION .....: Determine AccNum number AND cost centre
  APPLICATION ..: NN
  AUTHOR .......: aam
  CREATED ......: 07.03.2001
  MODIFIED .....: 09.09.03/aam brand
                  30.09.04/aam "own use" accounts for defined category
                  19.04.05/aam choose billitem account according to vatusage
                  24.04.18/skm removed the references of altaccnum and vipaccnum
 --------------------------------------------------------------------------- */

/* 
GetAccKeys:  get Account nbr AND cost accounting objects 
GetAccRcv :  get Account nbr FOR receivables from customer category
*/

DEF VAR xAccNum      AS INT  NO-UNDO.
DEF VAR xBillAccount AS INT  NO-UNDO.
DEF VAR xCostCentre  AS CHAR NO-UNDO.
DEF VAR xDepartment  AS CHAR NO-UNDO.
DEF VAR xDivision    AS CHAR NO-UNDO.
DEF VAR xCategory    AS CHAR NO-UNDO.
DEF VAR xPgcode      AS CHAR NO-UNDO.
DEF VAR lcOwnUse     AS CHAR NO-UNDO.

lcOwnUse = fCParamC("OwnUseCategory").

&IF "{&AccCust}" NE ""
&THEN
DEF BUFFER {&AccCust}Customer FOR Customer.
&ENDIF

FUNCTION GetAccKeys RETURNS LOGIC
    (icCategory AS CHAR,
     iVatUsage  AS INT,
     iItemCode  AS CHAR,
     iEventDate AS Date).
     
    DEFINE BUFFER bfCCRule FOR CCRule.

    ASSIGN xAccNum      = 0
           xBillAccount = 0 
           xCostCentre  = "".

    /* Account number  */
    IF NOT AVAILABLE BillItem OR BillItem.BillCode NE iItemCode 
    THEN FIND BillItem WHERE 
              BillItem.Brand    = Syst.Var:gcBrand AND
              BillItem.BillCode = iItemCode NO-LOCK NO-ERROR.
    
    IF AVAILABLE BillItem THEN DO:

        FIND FIRST bfCCRule NO-LOCK WHERE 
                   bfCCRule.Brand      =   BillItem.Brand      AND 
                  (bfCCRule.Category   =   icCategory  OR
                   bfCCRule.Category   =   "*" )               AND 
                   bfCCRule.BillCode   =   BillItem.BillCode   AND
                   bfCCRule.CLIType    =   ""                  AND
                   bfCCRule.ValidTo    >=  TODAY USE-INDEX Category  NO-ERROR.
                   
        IF AVAILABLE bfCCRule 
        THEN DO:          
            /* different sales account for own use */
            CASE iVATUsage:
            WHEN 0 OR
            WHEN 1 THEN xBillAccount  = bfCCRule.AccNum.
            WHEN 2 THEN xBillAccount  = bfCCRule.EUConAccNum.
            WHEN 3 THEN xBillAccount  = bfCCRule.EUAccNum.
            WHEN 4 THEN xBillAccount  = bfCCRule.FSAccNum.
            END CASE.
                            
            xAccNum = xBillAccount.
        END. /*  IF AVAILABLE bfCCRule*/
    END. /*  IF AVAILABLE BillIte*/

    RETURN TRUE.

END FUNCTION.

FUNCTION GetAccRcv RETURNS INTEGER
    (iCustNum AS INT,
     iKatkod  AS CHAR,
     iAccType AS INT).

    IF iKatkod = "" THEN DO:
        IF NOT AVAILABLE {&AccCust}Customer OR 
           {&AccCust}Customer.CustNum NE iCustNum 
        THEN FIND {&AccCust}Customer WHERE           
            {&AccCust}Customer.CustNum = iCustNum NO-LOCK NO-ERROR.
        IF AVAILABLE {&AccCust}Customer THEN 
            ASSIGN iKatkod = {&AccCust}Customer.Category.
    END.

    IF iKatkod NE "" THEN DO:
        
        FIND CustCat WHERE 
             CustCat.Brand    = Syst.Var:gcBrand AND
             CustCat.Category = iKatkod NO-LOCK NO-ERROR.

        IF AVAILABLE CustCat THEN DO:
            /* periodized, Unbilled OR normal receivable */
            CASE iAccType:
            WHEN 1 THEN RETURN(CustCat.UnbillAccNum).   /* Unbilled   */
            WHEN 2 THEN RETURN(CustCat.PerAccNum).  /* periodized */
            OTHERWISE RETURN (CustCat.ARAccNum).
            END CASE.
        END.
        
        ELSE RETURN (0).
    END.
    
    ELSE RETURN (0).

END FUNCTION.         



