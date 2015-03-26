/* spechead.i       15.06.04/aam

   common headers for specification reports
   
   callers:  nnpura2
             nnpura3
             nnpura4
             xmlinv
             
*/

{transname.i}

FUNCTION fSpecCLIHeader RETURNS CHARACTER
   (icCLI      AS CHAR,
    idtDate    AS DATE,
    iiLanguage AS INT).

   DEF VAR lcCLIHead AS CHAR NO-UNDO.
   
   /* Here we show either an unique A-sub no. OR a text "foreign A-sub"
      in case of freephone calls */
   IF icCLI BEGINS fri-b-prefix THEN 
      lcCLIHead = fTeksti(40,iiLanguage) + " -> " + icCLI. 

   ELSE lcCLIHead = fCLIHeader(icCLI,
                               idtDate).

   lcCLIHead = fTeksti(31,iiLanguage) + ": " + lcCLIHead.
   
   RETURN lcCLIHead.

END FUNCTION.

FUNCTION fSpecBItemHeader RETURNS CHARACTER
   (icBillCode  AS CHAR,
    iiLanguage  AS INT,
    idtSpecDate AS DATE).

   DEF VAR ldVAT       AS DEC  NO-UNDO.
   DEF VAR lcBItemHead AS CHAR NO-UNDO. 

   lcBItemHead = fTranslationName(gcBrand,
                                  1,
                                  icBillCode,
                                  iiLanguage,
                                  idtSpecDate).
                                  
   IF lcBItemHead = ? OR lcBItemHead = "" THEN DO:
      FIND BillItem WHERE 
           BillItem.Brand    = gcBrand AND
           BillItem.BillCode = icBillCode 
      NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem 
      THEN lcBItemHead = BillItem.BIName.
      ELSE lcBItemHead = icBillCode.
   END.
 

   RETURN lcBItemHead.
    
END FUNCTION.

FUNCTION fSpecCCNHeader RETURNS CHARACTER
   (iiCCN       AS INT,
    iiLanguage  AS INT,
    idtSpecDate AS DATE).
    
   DEF VAR lcCCNHead AS CHAR NO-UNDO. 

   /* country name  */
   lcCCNHead = fTranslationName(gcBrand,
                                3,
                                STRING(iiCCN),
                                iiLanguage,
                                idtSpecDate).
   
   if lcCCNHead = ? or lcCCNHead = "" THEN DO:
       FIND CCN WHERE 
            CCN.Brand = gcBrand AND
            CCN.CCN   = iiCCN
       NO-LOCK NO-ERROR. 
       IF AVAILABLE CCN THEN lcCCNHead = CCN.CCNName.
   END.

   RETURN lcCCNHead.
   
END FUNCTION.


