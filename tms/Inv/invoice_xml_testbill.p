/* ----------------------------------------------------------------------
  MODULE .......: invoice_xml_testbill.p
  TASK .........: Print one batch of test invoices to xml files 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 10.12.09
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

DEF INPUT PARAMETER idaInvDate AS DATE NO-UNDO.
DEF INPUT PARAMETER icBillRun  AS CHAR NO-UNDO.

DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR lcTestDir     AS CHAR NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO.

ASSIGN
   lcFile    = fCParamC("InvXMLFile")
   lcTestDir = fCParamC("InvXMLTestInvDir").
IF lcFile = "" OR lcFile = ? THEN RETURN "ERROR:Print file not defined".

/* print invoices to a directory named after the billing run id */
lcFile = lcTestDir + "/" + icBillRun + "*" + lcFile.
   
RUN printdoc1co ("",
                 0,
                 99999999,
                 "",
                 "ZZZZ",
                 idaInvDate,
                 FALSE,     /* only unprinted */
                 TRUE,      /* print credited */
                 99,        /* invoice type */
                 ?,         /* delivery type */
                 "XMLSEP" + "|" + icBillRun,  
                 lcFile,
                 FALSE,
                 OUTPUT liCount,
                 OUTPUT lcError).

RETURN lcError. 


