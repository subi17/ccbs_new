/* -----------------------------------------------
  MODULE .......: error_calldump.p
  FUNCTION .....: daily/error call dump
  ------------------------------------------------------ */

{commali.i}
katun = "cron".
gcBrand = "1".
{date.i}
{cparam2.i}


DEFINE VARIABLE idaDate AS DATE NO-UNDO. 
DEFINE VARIABLE lcParam AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldate1     AS DATE NO-UNDO.
DEFINE VARIABLE ldate2     AS DATE NO-UNDO.
DEFINE VARIABLE ldtDate    AS DATE NO-UNDO.
DEFINE VARIABLE lcFilename AS CHAR NO-UNDO.
DEFINE VARIABLE dformat    AS CHAR NO-UNDO. 
DEFINE VARIABLE lcOdir     AS CHAR NO-UNDO. 
DEFINE VARIABLE lcSdir    AS CHAR NO-UNDO.  
DEFINE VARIABLE lcNumForm    AS CHAR NO-UNDO. 

DEF VAR lcErrorName AS CHAR NO-UNDO. 

DEFINE STREAM lsErrorCDR.


lcParam = SESSION:PARAMETER.
IF lcParam NE "" THEN DO: 
  idaDate = DATE(lcParam) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO: 
     MESSAGE "Wrong date: " + lcParam.
     PAUSE 0.
     RETURN.
  END.
END.
ELSE idaDate = TODAY.

ASSIGN
   lcOdir     =  fCparamC("PentahoErrorCalls")
   lcSdir     =  fCParamC("PentahoSpool")
   ldate1     = idaDate
   lcFileName   = "error_calls" + fDateFmt(ldate1,"yyyymmdd") + "_" + 
                REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".dump"
   ldate1     = idaDate - 1
   ldate2     = ldate1
   lcNumForm    = SESSION:NUMERIC-FORMAT 
   SESSION:NUMERIC-FORMAT = "AMERICAN".

dformat = "yyyy-mm-dd".

DEFINE TEMP-TABLE ttSumErrorCDR
   FIELD CallDate   AS DATE
   FIELD ErrorCode  AS INT
   FIELD Qty        AS INT
   FIELD ErrorName  AS CHAR
   index CallDate is primary unique calldate ErrorCode.

FUNCTION fUpdateSummary RETURN LOGICAL
         (INPUT pdtCDRDate AS DATE,
          INPUT piCDRError AS INT):

       FIND ttSumErrorCDR WHERE
            ttSumErrorCDR.CallDate = pdtCDRDate AND
            ttSumErrorCDR.ErrorCode = piCDRError EXCLUSIVE-LOCK NO-ERROR. 

       IF NOT AVAIL ttSumErrorCDR THEN DO:

            lcErrorName = "".
            FIND MobError WHERE 
                 MobError.MobError = piCDRError NO-LOCK NO-ERROR. 
             IF AVAIL MobError THEN lcErrorName = MobError.MEName .

            CREATE ttSumErrorCDR. 
            ASSIGN ttSumErrorCDR.CallDate = pdtCDRDate 
                   ttSumErrorCDR.ErrorCode = piCDRError
                   ttSumErrorCDR.ErrorName = lcErrorName.
       END.

       ASSIGN ttSumErrorCDR.Qty = ttSumErrorCDR.Qty + 1 .

       RELEASE ttSumErrorCDR. 

RETURN TRUE.
END FUNCTION.

FOR EACH MobCDR NO-LOCK USE-INDEX ReadDate WHERE
         MobCDR.ReadDate >= ldate1 AND
         MobCDR.ReadDate <= ldate2 AND
         MobCDR.ErrorCode > 0:
     
   fUpdateSummary(MobCDR.DateSt,
                  MobCDR.ErrorCode).
            
END.

FOR EACH ErrorCDR NO-LOCK USE-INDEX ReadDate WHERE
         ErrorCDR.ReadDate >= ldate1 AND
         ErrorCDR.ReadDate <= ldate2:

   fUpdateSummary(ErrorCDR.DateSt,
                  ErrorCDR.ErrorCode).
END.

OUTPUT STREAM lsErrorCDR  TO VALUE(lcSdir + "/" + lcFileName).

FOR EACH ttSumErrorCDR NO-LOCK :
  PUT STREAM lsErrorCDR UNFORMATTED 
        STRING(ttSumErrorCDR.CallDate) + "|" + 
        STRING(ttSumErrorCDR.ErrorCode) + "|" + 
        ttSumErrorCDR.ErrorName + "|" + 
        STRING(ttSumErrorCDR.Qty) SKIP.
END.

OUTPUT STREAM lsErrorCDR CLOSE.

UNIX SILENT VALUE("mv " + lcSdir + "/" + lcFileName + " " + lcODir).

SESSION:NUMERIC-FORMAT = lcNumForm.

