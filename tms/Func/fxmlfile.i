/* fxmlfile.i           09.05.03/aam

   name for xml/pdf-files 
*/


FUNCTION fXMLInvFileName RETURNS CHARACTER
   (icExtension AS CHAR).

   DEF VAR lcXMLDir  AS CHAR NO-UNDO. 

   /* directory for invoice xml -files */
   lcXMLDir = OS-GETENV("INVXMLDIR").
   IF lcXMLDir = ? THEN lcXMLDir = "".
   ELSE DO:
      IF SUBSTRING(lcXMLDir,LENGTH(lcXMLDir),1) NE "/"
      THEN lcXMLDir = lcXMLDir + "/".
   END.

   IF icExtension = "" THEN icExtension = ".xml".

   /* file name */
   RETURN lcXMLDir                             + 
          'inv_'                               +
          STRING(Invoice.CustNum)              + "_" +
          STRING(YEAR(Invoice.InvDate),"9999") +
          STRING(MONTH(Invoice.InvDate),"99")  +
          STRING(DAY(Invoice.InvDate),"99")    + "_" +
          STRING(Invoice.InvNum)               + 
          icExtension.

END FUNCTION.

FUNCTION fXMLRepFileName RETURNS CHARACTER
   (iiCustNum   AS INT,
    icRepName   AS CHAR,
    icExtension AS CHAR).

   DEF VAR lcXMLDir  AS CHAR NO-UNDO. 

   /* directory for xml -files */
   lcXMLDir = OS-GETENV("XMLDIR").
   IF lcXMLDir = ? THEN lcXMLDir = "".
   ELSE DO:
      IF SUBSTRING(lcXMLDir,LENGTH(lcXMLDir),1) NE "/"
      THEN lcXMLDir = lcXMLDir + "/".
   END.

   IF icExtension = "" THEN icExtension = ".xml".
   IF icRepName = "" THEN icRepName = "report".

   /* file name */
   RETURN lcXMLDir                           + 
          'rep_'                             +
          STRING(iiCustNum)                  + "_" +
          STRING(YEAR(TODAY),"9999")         +
          STRING(MONTH(TODAY),"99")          +
          STRING(DAY(TODAY),"99")            + "_" +
          icRepName                          + 
          icExtension.

END FUNCTION.

