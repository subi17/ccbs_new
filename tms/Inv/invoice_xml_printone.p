/* ----------------------------------------------------------------------
  MODULE .......: invoice_xml_printone.p
  TASK .........: Print one invoice to an xml file 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.10.09
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}

DEF INPUT PARAMETER iiInvNum   AS INT  NO-UNDO.
DEF INPUT PARAMETER icTransDir AS CHAR NO-UNDO.
DEF INPUT PARAMETER icFile     AS CHAR NO-UNDO.

DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO.

IF icFile = "" THEN lcFile = fCParamC("InvXMLFile").
ELSE lcFile = icFile.

IF lcFile = "" OR lcFile = ? THEN RETURN "ERROR:Print file not defined".

IF icTransDir > "" THEN 
   lcFile = icTransDir + "*" + lcFile.
   
FIND FIRST Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Invoice THEN RETURN "ERROR:Invoice not found".

RUN printdoc1co ("",
                 Invoice.CustNum,
                 Invoice.CustNum,
                 Invoice.ExtInvID,
                 Invoice.ExtInvID,
                 Invoice.InvDate,
                 FALSE,     /* only unprinted */
                 TRUE,      /* print credited */
                 Invoice.InvType,
                 Invoice.DelType,
                 "XMLSEP",  
                 lcFile,
                 FALSE,
                 OUTPUT liCount,
                 OUTPUT lcError).

RETURN lcError. 


