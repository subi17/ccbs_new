/* ----------------------------------------------------------------------
  MODULE .......: taxreportb.p
  TASK .........: Print a tax report from invoices, batch-version
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 28.03.07
  CHANGED ......: 25.04.07/aam also upper limit for InvType
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/lib/eventlog.i}

DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR ldtInvDate    AS DATE NO-UNDO EXTENT 2.
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR liFiles       AS INT  NO-UNDO.


ASSIGN 
   ldtInvDate[2]   = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
   ldtInvDate[1]   = DATE(MONTH(ldtInvDate[2]),1,YEAR(ldtInvDate[2]))
   lcFile          = fCParamC("TaxRepFileName").
       

IF lcFile = "" OR lcFile = ? THEN DO:
   fELog("TAXREPORT","PostPaid:ERRORFile_name_has_not_been_defined").
   RETURN.
END.

fELog("TAXREPORT","PostPaidStarted").

RUN Ar/taxreport ("",             /* TaxZone */
               "",             /* CustID */
               0,              /* InvType */
               98,             /* InvType, no test invoices */
               ldtInvDate[1],
               ldtInvDate[2],
               lcFile,
               OUTPUT liCount,
               OUTPUT liFiles,
               OUTPUT lcError).

fELog("TAXREPORT","PostPaidEnded" +
                  ":Files" +  STRING(liFiles) +
                  ":Invoices" + STRING(liCount) +
                  (IF lcError > ""
                   THEN ":ERROR" + lcError
                   ELSE "")).


