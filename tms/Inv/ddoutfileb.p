/* ---------------------------------------------------------------------------
  MODULE .......: DDOUTFILEB.P
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for printing invoices to a csb19 (dd) file     
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 16.04.07
  CHANGED ......:
  Version ......: M15
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "csb19b".
       
{Func/cparam2.i}
{Syst/eventlog.i}

DEF VAR liCount    AS INT  NO-UNDO.
DEF VAR liFiles    AS INT  NO-UNDO.
DEF VAR lcError    AS CHAR NO-UNDO.
DEF VAR lcFile     AS CHAR NO-UNDO.
DEF VAR lcTransDir AS CHAR NO-UNDO.

DEF STREAM sRead.

lcFile = fCParamC("DDebitBatchFile").
IF lcFile = ? OR lcFile = "" THEN DO:
   fELog("DDOUT","ERROR:FileNotDefined").
   RETURN.
END.

fELog("DDOUT","Started").

RUN Inv/ddoutfileco.p ("",                   /* InvGroup  */
                 0,                    /* customers from */
                 999999999,             /* customers to   */
                 "",                   /* invoices from  */
                 "ZZZZZ",              /* invoices to    */
                 TODAY,                /* invdate   */
                 1,                    /* normal invoice type */
                 "",                   /* billrun not limited */
                 1,                    /* print state from    */
                 1,                    /* print state to      */
                 lcFile,
                 TRUE,                 /* make file even if empty */
                 TRUE,                 /* split the file */
                 0,
                 0,
                 FALSE,                /* schema validation disabled */
                 "",                   /* input files dir */
                 OUTPUT liCount,
                 OUTPUT liFiles,
                 OUTPUT lcError).
 

fELog("DDOUT","Stopped:" + STRING(liCount) +
                  (IF lcError > "" THEN ":" + lcError ELSE "")).


