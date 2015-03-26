
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: timok 
  CREATED ......: 07.02.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
DEFINE VARIABLE   llPrint AS LOGICAL NO-UNDO.   

MESSAGE  "Print BillingEvent report to CSV 
          file (/scratch/reports/eventrep.csv) ?" 
          VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE llPrint. 

IF llPrint THEN DO:

OUTPUT TO /scratch/reports/eventrep.csv.
FOR EACH Feemodel no-lock:
   EXPORT Feemodel.Feemodel Feemodel.Feename.
END.
OUTPUT CLOSE.

END.

