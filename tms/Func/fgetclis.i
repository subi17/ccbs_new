/* fgetclis.i       20.01.04/aam separated from nnpura3.i
*/

DEF TEMP-TABLE wCLI NO-UNDO
    FIELD CLI     AS CHAR 
    FIELD CustNum AS INT
    FIELD Owner   AS CHAR 
    FIELD OwnerID AS INT
    FIELD Country AS CHAR
    INDEX CLI is unique CLI.

DEF BUFFER bCLICust FOR Customer.

FUNCTION fGetCLIs RETURNS LOGICAL
   (icCLI AS CHAR).
            
   DEF VAR liFromPer  AS DEC  NO-UNDO. 
   DEF VAR liToPer    AS DEC  NO-UNDO. 
   DEF VAR ldtCLIFrom AS DATE NO-UNDO.
   DEF VAR ldtCLITo   AS DATE NO-UNDO.
   
   EMPTY TEMP-TABLE wCLI.
   
   /* get ALL cli:s */
   FOR EACH SubInvoice no-lock where
            SubInvoice.InvNum = Invoice.InvNum AND
            (IF icCLI > "" 
             THEN SubInvoice.CLI = icCLI
             ELSE TRUE):

         ASSIGN 
            ldtCLIFrom = IF Invoice.FirstCall NE ? 
                         THEN Invoice.FirstCall
                         ELSE Invoice.FromDate
            ldtCLITo   = Invoice.ToDate.
         
         IF CAN-FIND(FIRST wCLI WHERE wCLI.CLI = SubInvoice.CLI) THEN NEXT.
         
         CREATE wCLI.
         ASSIGN wCLI.CLI = SubInvoice.CLI.

         IF ldtCLIFrom = ? THEN ldtCLIFrom = Invoice.FromDate.
         IF ldtCLiTo   = ? THEN ldtCLITo   = Invoice.ToDate.
         
         ASSIGN liFromPer = YEAR(ldtCliFrom) * 10000 +
                            MONTH(ldtCLiFrom) * 100  +
                            DAY(ldtCliFrom) + 0.86399
                liToPer   = YEAR(ldtCliTo) * 10000 +
                            MONTH(ldtCLiTo) * 100  +
                            DAY(ldtCliTo) +
                            (IF ldtCLIFrom = ldtCLITo
                             THEN 0.86399
                             ELSE 0).

         /* FIRST try TO FIND a mobile cli */
         FOR FIRST MSOwner no-lock where 
                   MSOwner.Brand    = gcBrand AND
                   MSOwner.CLI      = SubInvoice.CLI AND
                   MSOwner.TSBegin <= liToPer     AND
                   MSOwner.TSEnd   >= liFromPer,
             FIRST bCLICust NO-LOCK WHERE
                   bCLICust.CustNum = MsOwner.CustNum:

             ASSIGN wCLI.CustNum = MSOwner.CustNum
                    wCLI.OwnerID = RECID(MSOwner)
                    wCLI.Owner   = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                    BUFFER bCLICust).
         END.


         /* FIND fixed cli IF mobile was NOT found */
         IF wCLI.CustNum = 0 THEN 
         FOR FIRST CLI no-lock where 
                   CLI.CLI      = SubInvoice.CLI AND
                   CLI.CrStamp <= liToPer     AND
                   CLI.ClStamp >= liFromPer:
              ASSIGN wCLI.CustNum = CLI.CustNum 
                     wCLI.OwnerID = -1 * INTEGER(RECID(CLI)).
         END.

         /* mark to invoicing customer if no clis were found */
         IF wCLI.CustNum = 0 THEN wCLI.CustNum = Invoice.CustNum.

   END.      

            
END FUNCTION.            
            
 
