/* nnpura3.i       28.04.03/aam 

   callers: nnpura3.p       
            xmlinv.p
            
                   22.10.03/aam use timestamp for MSOwner
                   27.10.03/aam vat header to product level               
                   18.12.03/aam row headers to 2 lines 
                   15.06.04/aam some functions moved to spechead.i
                   11.04.05/aam new columns -> more lcASubHeaders
            
*/
            
DEF VAR liASubLang       AS INT  NO-UNDO.
DEF VAR lcASubASubHeader AS CHAR NO-UNDO.
DEF VAR lcASubProdHeader AS CHAR NO-UNDO.
DEF VAR lcASubCCNHeader  AS CHAR NO-UNDO.
DEF VAR liASubCnt        AS INT  NO-UNDO.
DEF VAR liASubPoint      AS INT  NO-UNDO. 
DEF VAR lcASubDateHeader AS CHAR NO-UNDO. 
DEF VAR lcASubHeader     AS CHAR NO-UNDO EXTENT 20. 

def var lcASubVAT        as char no-undo format "x(40)".

DEF BUFFER bRep3Cust FOR Customer.

FUNCTION fSetASubSubHeaders RETURN LOGICAL
   (idtDate1 AS DATE,
    idtDate2 AS DATE). 

   lcASubDateHeader = STRING(idtDate1,"99.99.9999") + " - " +
                      STRING(idtDate2,"99.99.9999").

END FUNCTION.

FUNCTION fSetASubCustHeaders RETURNS LOGICAL.

   ASSIGN liASubLang = Customer.Language
          erisivu    = index(Customer.RepCodes,"-") > 0.

   ASSIGN lcASubHeader     = "" 
          lcASubHeader[1]  = fTeksti(23,liASubLang)
          lcASubHeader[2]  = fTeksti(28,liASubLang)
          lcASubHeader[3]  = fTeksti(27,liASubLang)
          lcASubHeader[4]  = fTeksti(29,liASubLang)
          lcASubHeader[5]  = fTeksti(95,liASubLang)
          lcASubHeader[6]  = fTeksti(15,liASubLang)
          lcASubHeader[7]  = IF Invoice.VatIncl = FALSE
                             THEN fTeksti(150,liASubLang)
                             ELSE fTeksti(151,liASubLang)
          lcASubHeader[8]  = fTeksti(48,liASubLang)
          lcASubHeader[9]  = fTeksti(192,liASubLang)
          lcASubHeader[10] = fTeksti(193,liASubLang)
          lcSpecDateHead   = fTeksti(136,liASubLang).

   /* divide headers to 2 lines */
   DO liASubCnt = 3 TO 10:
      liASubPoint = INDEX(lcASubHeader[liASubCnt],"!").
      IF liASubPoint > 1 THEN ASSIGN 
         lcASubHeader[liAsubCnt + 10] = SUBSTRING(lcASubHeader[liASubCnt],1,
                                                  liASubPoint - 1)
         lcASubHeader[liASubCnt]      = SUBSTRING(lcASubHeader[liASubCnt],
                                                  liASubPoint + 1).
   END.
      
   ASSIGN 
   lcAsubHeader[5]  = FILL(" ",7 - LENGTH(lcASubHeader[5])) + lcASubHeader[5]
   lcAsubHeader[15] = FILL(" ",7 - LENGTH(lcASubHeader[15])) +
                      lcASubHeader[15]
   
   lcAsubHeader[7]  = FILL(" ",11 - LENGTH(lcASubHeader[7])) + lcASubHeader[7]
   lcAsubHeader[17] = FILL(" ",11 - LENGTH(lcASubHeader[17])) + 
                      lcASubHeader[17]

   lcAsubHeader[9]  = FILL(" ",11 - LENGTH(lcASubHeader[9])) + lcASubHeader[9]
   lcAsubHeader[19] = FILL(" ",11 - LENGTH(lcASubHeader[19])) + 
                      lcASubHeader[19]

   lcAsubHeader[10] = FILL(" ",11 - LENGTH(lcASubHeader[10])) + 
                      lcASubHeader[10]
   lcAsubHeader[20] = FILL(" ",11 - LENGTH(lcASubHeader[20])) + 
                      lcASubHeader[20]

   lcEPLRepHead = fTeksti(57,LIkieli).
   
END FUNCTION.


  
