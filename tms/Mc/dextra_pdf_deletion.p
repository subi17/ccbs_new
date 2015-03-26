/* ----------------------------------------------------------------------
  MODULE .......: dextra_pdf_deletion.p
  TASK .........: PDF contract generated during the order process needs 
                  to be removed if order is cancelled before sending it
                  to Dextra
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Wed Sep 03 15:43:40 EEST 2014
  Version ......: Yoigo
----------------------------------------------------------------------- */

/* ***************************  Definitions  ************************** */
{commpaa.i}
katun = "Cron".
gcBrand = "1".

{tmsconst.i}
{cparam2.i}
{timestamp.i}
{forderstamp.i}

DEFINE VARIABLE lcIncDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFilename   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE liFilelength AS INTEGER   NO-UNDO.
DEFINE VARIABLE liOrderId    AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeTimeStamp AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldtCloseDate AS DATE      NO-UNDO.
DEFINE VARIABLE llgClose     AS LOGICAL   NO-UNDO.

DEFINE STREAM ipstream.
/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* Every Monday at 7 A.M */

ASSIGN lcIncDir = fCParam("Logistics","ContractsDir"). 
       
INPUT STREAM ipstream THROUGH VALUE("ls -1tr " + lcIncDir).

REPEAT:
   IMPORT STREAM ipstream UNFORMATTED lcFilename.

   ASSIGN liFilelength = LENGTH(lcFilename) - 4
          liOrderId    = INTEGER(SUBSTRING(lcFilename,1,liFilelength))
          lcInputFile  = lcIncDir + lcFilename NO-ERROR.
   IF ERROR-STATUS:ERROR THEN NEXT.
        
   FOR FIRST Order WHERE 
            Order.Brand      = gcBrand AND
            Order.OrderId    = liOrderId                       AND
           (Order.StatusCode = {&ORDER_STATUS_CLOSED}          OR
            Order.StatusCode = {&ORDER_STATUS_CLOSED_BY_FRAUD} OR
            Order.StatusCode = {&ORDER_STATUS_AUTO_CLOSED})    NO-LOCK:

      IF Order.logistics > "" THEN NEXT.
      
      ASSIGN ldeTimeStamp = fGetOrderStamp(Order.OrderId,"Close").
      IF ldeTimeStamp eq 0 THEN NEXT.

      llgClose = fTS2Date(ldeTimeStamp, OUTPUT ldtCloseDate).
      IF llgClose EQ FALSE THEN NEXT.

      IF Order.OrderChannel BEGINS "Retention"  AND
         ldtCloseDate <= ADD-INTERVAL(TODAY,-6,"months")  THEN
         OS-DELETE VALUE(lcInputFile).

      ELSE IF NOT Order.OrderChannel BEGINS "Retention" AND
         ldtCloseDate <= ADD-INTERVAL(TODAY,-1,"months") THEN
            OS-DELETE VALUE(lcInputFile).

   END.
END.
    
INPUT STREAM ipstream CLOSE.
