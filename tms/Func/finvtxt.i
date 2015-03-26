/* finvtxt.i        10.11.04/aam 
*/


FUNCTION fGetInvTextID RETURNS INTEGER
   (icTarget   AS CHAR,
    icKey      AS CHAR,
    iiLanguage AS INT,
    idtDate    AS DATE).
    
   DEF VAR liTxtID AS INT NO-UNDO. 

   liTxtID = 0.

   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand     AND
             InvText.Target    = icTarget    AND
             InvText.KeyValue  = icKey       AND
             InvText.Language  = iiLanguage  AND 
             InvText.FromDate <= idtDate     AND
             InvText.ToDate   >= idtDate:

      liTxtID = InvText.ITNum.
   END.

   RETURN liTxtID.
   
END FUNCTION.

