/* ffeecont.i       22.03.04/aam 

   create a contract when fee is created
*/

&IF "{&ffeecont}" NE "YES"
          &THEN
          
          &GLOBAL-DEFINE ffeecont YES
          
FUNCTION fFeeContract RETURNS CHARACTER
   (icBrand    AS CHAR,
    iiCustNum  AS INT,
    icSalesman AS CHAR,
    idtDate    AS DATE,
    icMemo     AS CHAR).
    
   DEF VAR lcContractID AS CHAR NO-UNDO.

   lcContractID = "".
   
   IF icSalesman = "" THEN DO:
      /* get user's salesman definition */
      FIND UserSman WHERE 
           UserSman.UserCode = katun AND
           UserSman.Brand    = icBrand NO-LOCK NO-ERROR.
   
      IF AVAILABLE UserSman THEN icSalesman = UserSman.Salesman.
   END. 
     
   IF icSalesman > "" THEN DO:
   
      FIND FIRST Contract WHERE
                 Contract.Brand     = icBrand    AND
                 Contract.Salesman  = icSalesman AND
                 Contract.CustNum   = iiCustNum  AND
                 Contract.FromDate <= idtDate    AND
                 Contract.ToDate   >= idtDate NO-LOCK NO-ERROR.
                 
      IF AVAILABLE Contract THEN lcContractID = Contract.Contract.
      
      ELSE DO:
      
         CREATE Contract.
         ASSIGN Contract.Contract  = STRING(NEXT-VALUE(ContractID))
                Contract.Brand     = icBrand 
                Contract.Salesman  = icSalesman
                Contract.CustNum   = iiCustNum
                Contract.FromDate  = idtDate
                Contract.ToDate    = 12/31/2049
                Contract.ContrType = 1
                Contract.CommPerm  = TRUE
                Contract.Memo      = icMemo
                lcContractID       = Contract.Contract.
    
         RELEASE Contract. 
         
      END.
   END. 

   RETURN lcContractID.
 
END FUNCTION.


&ENDIF
