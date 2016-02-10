/* ---------------------------------------------------------------------------
  MODULE .......: REFUNDFILEB.P
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for making a refund file     
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 01.11.07
  CHANGED ......: 
  Version ......: 
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "rfndfile".
       
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/timestamp.i}

DEF VAR liCount    AS INT  NO-UNDO.
DEF VAR liFiles    AS INT  NO-UNDO.
DEF VAR lcError    AS CHAR NO-UNDO.
DEF VAR lcFile     AS CHAR NO-UNDO.

DEF STREAM sRead.

lcFile = fCParamC("RefundFile").
IF lcFile = ? OR lcFile = "" THEN RETURN.

fELog("REFUNDFILE","Started").

RUN Ar/refundfileco ("",                 /* InvGroup  */
                  0,                  /* customers from */
                  99999999,           /* customers to   */
                  TODAY,              /* payment date */
                  lcFile,
                  FALSE,              /* no empty file */
                  OUTPUT liCount,
                  OUTPUT liFiles,
                  OUTPUT lcError).
 
fELog("REFUNDFILE","Stopped:" +
      STRING(liFiles) + "Files," +
      STRING(liCount) + "Payments" +
      (IF lcError > "" THEN ":" + lcError ELSE "")).


