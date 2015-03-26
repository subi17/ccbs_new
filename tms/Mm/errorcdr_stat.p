/* ----------------------------------------------------------------------
  MODULE .......: errorcdr_stat.p
  TASK .........: Print a report of errorneus cdrs 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 30.04.09
  Version ......: yoigo
---------------------------------------------------------------------- */

&GLOBAL-DEFINE TraceLog NO

{commali.i}
{timestamp.i}
{cparam2.i}
{ftransdir.i}
{coinv.i}
{callquery.i}

&IF "{&TraceLog}" = "YES" 
&THEN
{log.i}
fSetLogFileName("/tmp/errorcdr_stat.log").
fSetLogEntryTypes("4GLTrace:4").
fClearLog().
&ENDIF 

DEF INPUT  PARAMETER idaCDRDate1 AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaCDRDate2 AS DATE NO-UNDO.
DEF INPUT  PARAMETER icFile      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icTransDir  AS CHAR NO-UNDO. 
DEF OUTPUT PARAMETER oiCDRCount  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError     AS CHAR NO-UNDO.


DEF VAR lcFile      AS CHAR NO-UNDO.
DEF VAR lcSkipCodes AS CHAR NO-UNDO.

DEF TEMP-TABLE ttError NO-UNDO
   FIELD EventDate AS DATE
   FIELD ErrorCode AS INT
   FIELD Qty       AS INT
   INDEX ErrorCode ErrorCode EventDate.

DEF TEMP-TABLE ttCall NO-UNDO LIKE MobCDR
   FIELD CDRTable AS CHAR.

DEF STREAM sFile.


FUNCTION fDispDecimal RETURNS CHARACTER
   (idAmt AS DEC):
   
   RETURN TRIM(STRING(idAmt,"->>>>>>>>9.99")).
      
END FUNCTION.


/******** Main start ******/      

RUN pInitialize.

RUN pCollectCDRs.

RUN pPrintReport.

&IF "{&TraceLog}" = "YES" 
&THEN
fCloseLog().
&ENDIF

RETURN "".

/******** Main end  ******/


PROCEDURE pInitialize:

   IF icTransDir = ? THEN icTransDir = "".

   ASSIGN 
      lcSkipCodes = fCParamC("ErrorCDRStatSkipCode")
      lcFile = REPLACE(icFile,"#PERIOD",STRING(YEAR(idaCDRDate2),"9999") +
                                        STRING(MONTH(idaCDRDate2),"99"))
      lcFile = REPLACE(lcFile,"#DATE",STRING(YEAR(idaCDRDate2),"9999") +
                                      STRING(MONTH(idaCDRDate2),"99")  +
                                      STRING(DAY(idaCDRDate2),"99")).
      
END PROCEDURE.

PROCEDURE pCollectCDRs:

   DEFINE VARIABLE tthCDR         AS HANDLE    NO-UNDO.
   DEFINE VARIABLE liErrorCodeOut AS INT       NO-UNDO.

   tthCDR = TEMP-TABLE ttCall:HANDLE.

   FOR EACH MobError NO-LOCK WHERE
            LOOKUP(STRING(MobError.MobError),lcSkipCodes) = 0:

      EMPTY TEMP-TABLE ttCall.
               
      fMobCDRCollect(INPUT "post",
                     INPUT gcBrand,
                     INPUT katun,
                     INPUT idaCDRDate1,
                     INPUT idaCDRDate2,
                     INPUT 0,
                     INPUT "",
                     INPUT "",
                     INPUT 0,
                     INPUT 0,
                     INPUT "",
                     INPUT "",
                     INPUT "",
                     INPUT MobError.MobError,
                     INPUT-OUTPUT liErrorCodeOut,
                     INPUT-OUTPUT tthCDR).
            
      FOR EACH ttCall:

         FIND FIRST ttError WHERE
                    ttError.ErrorCode = ttCall.ErrorCode AND
                    ttError.EventDate = ttCall.DateSt NO-ERROR.
         IF NOT AVAILABLE ttError THEN DO:
            CREATE ttError.
            ASSIGN 
               ttError.ErrorCode = ttCall.ErrorCode
               ttError.EventDate = ttCall.DateSt.
         END.
      
         ttError.Qty = ttError.Qty + 1.
             
         &IF "{&TraceLog}" = "YES" 
         &THEN
         fLogBasic("Error:" + STRING(ttCall.ErrorCode)).
         &ENDIF
        
         oiCDRCount = oiCDRCount + 1.

         IF NOT SESSION:BATCH AND oiCDRCount MOD 1000 = 0 THEN DO:
            PAUSE 0.
            DISP oiCDRCount  
                    LABEL "CDR Qty" 
                    FORMAT ">>>>>>>9"
                    SKIP
                 MobError.MobError   
                    LABEL "Error"
            WITH SIDE-LABELS 1 DOWN ROW 10 CENTERED OVERLAY 
               TITLE " ERROR CDRS " FRAME fCDR.
         END.    
      END.
      
   END.

   IF NOT SESSION:BATCH THEN HIDE FRAME fCDR NO-PAUSE.

   EMPTY TEMP-TABLE ttCall.
   IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.
    
END PROCEDURE. /* pCollectCDRs */

PROCEDURE pPrintReport:

   DEF VAR liCnt          AS INT  NO-UNDO.
   
   OUTPUT STREAM sFile TO VALUE(lcFile).

   PUT STREAM sFile UNFORMATTED
      "ERROR CALLS REPORT" SKIP
      "Reporting date"           CHR(9)
         STRING(TODAY,"99.99.9999") SKIP(1)
      "ERROR CODE" CHR(9)
         "NAME"    CHR(9)
         "DATE"    CHR(9)
         "QTY"     CHR(9)
         "TOTAL QTY" SKIP.

   FOR EACH ttError 
   BREAK BY ttError.ErrorCode 
         BY ttError.EventDate:
         
      IF FIRST-OF(ttError.ErrorCode) THEN DO:
         FIND FIRST MobError WHERE MobError.MobError = ttError.ErrorCode
            NO-LOCK NO-ERROR.

         PUT STREAM sFile UNFORMATTED
            ttError.ErrorCode  CHR(9)
            (IF AVAILABLE MobError THEN MobError.MEName ELSE "") CHR(9).
      END.
      
      ELSE PUT STREAM sFile UNFORMATTED
         CHR(9) CHR(9).
         
      PUT STREAM sFile UNFORMATTED
         STRING(ttError.EventDate,"99.99.9999") CHR(9)
         ttError.Qty SKIP.
         
      ACCUMULATE ttError.Qty (TOTAL BY ttError.ErrorCode).
      
      IF LAST-OF(ttError.ErrorCode) THEN DO:
         PUT STREAM sFile UNFORMATTED
            CHR(9) CHR(9) 
            "TOTAL"        CHR(9)
            CHR(9)
            (ACCUM TOTAL BY ttError.ErrorCode ttError.Qty) SKIP(1).
      END.
      
      IF LAST(ttError.ErrorCode) THEN DO:
         PUT STREAM sFile UNFORMATTED
            "TOTAL"        CHR(9)
            CHR(9) CHR(9)
            (ACCUM TOTAL ttError.Qty) SKIP.
      END.
   END.
   
   OUTPUT STREAM sFile CLOSE.

   /* move the file to the transfer directory */
   IF icTransDir > "" THEN DO:
      fTransDir(lcFile,
                ".txt",
                icTransDir).
   END.

END PROCEDURE. /* pPrintReport */

