/* ----------------------------------------------------------------------
  MODULE .......: taxvouchrepb.p
  TASK .........: Print a tax report from topup payments, batch-version
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 28.03.07
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/lib/eventlog.i}

DEF VAR liCount      AS INT  NO-UNDO. 
DEF VAR lcFile       AS CHAR NO-UNDO.
DEF VAR ldtAccDate   AS DATE NO-UNDO EXTENT 2.
DEF VAR lcError      AS CHAR NO-UNDO. 
DEF VAR liFiles      AS INT  NO-UNDO.


ASSIGN 
   ldtAccDate[2] = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
   ldtAccDate[1] = DATE(MONTH(ldtAccDate[2]),1,YEAR(ldtAccDate[2]))
   lcFile        = fCParamC("TaxVouchFileName").
       

IF lcFile = "" OR lcFile = ? THEN DO:
   fELog("TAXREPORT","PrePaid:ERRORFile_name_has_not_been_defined").
   RETURN.
END.

fELog("TAXREPORT","PrePaidStarted").

RUN Ar/taxvouchrep ("",             /* TaxZone */
                 "",             /* CustID */
                 0,              /* PaymType */
                 ldtAccDate[1],
                 ldtAccDate[2],
                 lcFile,
                 OUTPUT liCount,
                 OUTPUT liFiles,
                 OUTPUT lcError).

fELog("TAXREPORT","PrePaidEnded" +
                  ":Files" +  STRING(liFiles) +
                  ":Payments" + STRING(liCount) +
                  (IF lcError > ""
                   THEN ":ERROR" + lcError
                   ELSE "")).


