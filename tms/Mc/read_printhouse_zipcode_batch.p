/* ---------------------------------------------------------------------------
  MODULE .......: read_printhouse_zipcode_batch.p
  FUNCTION .....: batch process for reading printhouse zipcodes from files         SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 17.03.11
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "Cron".
       
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/timestamp.i}

DEF VAR lcFile      AS CHAR NO-UNDO.
DEF VAR liRead      AS INT  NO-UNDO. 
DEF VAR liError     AS INT  NO-UNDO.
DEF VAR liFiles     AS INT  NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR liCnt       AS INT  NO-UNDO.

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD ZipCodeFile AS CHAR
   INDEX ZipCodeFile ZipCodeFile.

DEF STREAM sRead.

FUNCTION fCollTemp RETURNS LOGIC
   (icZipCodeFile AS CHAR).

   /* file not found */
   IF icZipCodeFile = "" OR SEARCH(icZipCodeFile) = ? THEN NEXT.
   
   IF CAN-FIND(FIRST ttFiles WHERE ttFiles.ZipCodeFile = icZipCodeFile) THEN
      NEXT.
   
   CREATE ttFiles.
   ASSIGN ttFiles.ZipCodeFile = icZipCodeFile.
   
END FUNCTION.


/******** Main start ******/

FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Company THEN ynimi = Company.CompName.

lcFile = fCParamC("PHouseZipCodeFile").
   
IF lcFile = "" OR lcFile = ? THEN RETURN "ERROR:Definitions missing".
                                       
fELog("READPHZIPCODE","started").

RUN pFindFiles (lcFile,
                OUTPUT lcFile).
                      
IF lcFile > "" THEN 
DO liCnt = 1 TO NUM-ENTRIES(lcFile,"¤"):
   fCollTemp(ENTRY(liCnt,lcFile,"¤")).
END.

      
FOR EACH ttFiles:

   liFiles = liFiles + 1.
   
   RUN Mc/read_printhouse_zipcode.p (ttFiles.ZipCodeFile,
                                  OUTPUT liRead,
                                  OUTPUT liError).
   
END.

fELog("READPHZIPCODE","stopped,Files:" + STRING(liFiles)).

/******** Main end ******/


PROCEDURE pFindFiles:

   DEF INPUT  PARAMETER icFilter AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER ocFiles  AS CHAR NO-UNDO. 
       
   DEF VAR lcFileName AS CHAR NO-UNDO.
   
   INPUT STREAM sRead THROUGH VALUE("ls -1 " + icFilter).

   REPEAT:
      IMPORT STREAM sRead UNFORMATTED lcFileName.
      
      IF lcFileName = "" OR 
         lcFileName MATCHES ("*o such file or*") 
      THEN NEXT.
      
      ocFiles = ocFiles + 
                (IF ocFiles > "" THEN "¤" ELSE "") + 
                lcFileName.
   END.
                        
   INPUT STREAM sRead CLOSE.

END PROCEDURE.



