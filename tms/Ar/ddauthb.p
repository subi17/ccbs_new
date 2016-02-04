/* ---------------------------------------------------------------------------
  MODULE .......: DDAUTHB.P
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for reading dd authorizations from files       
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 30.05.05
  CHANGED ......:
  Version ......: M15
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "ddauth".

{Syst/utumaa.i "new"}
{Func/cparam2.i}
{Ar/ddtrans.i}
{Func/lib/eventlog.i}

ASSIGN tuni1 = "nnsvlu"
       tuni2 = "".

DEF VAR lcInFile   AS CHAR  NO-UNDO FORMAT "x(55)".
DEF VAR lcDir      AS CHAR  NO-UNDO.
DEF VAR lcFile     AS CHAR  NO-UNDO. 
DEF VAR lcFromDir  AS CHAR  NO-UNDO. 
DEF VAR lcChoose   AS CHAR  NO-UNDO. 
DEF VAR liCount    AS INT   NO-UNDO. 
DEF VAR lcAuthFile AS CHAR NO-UNDO.
DEF VAR liRead     AS INT  NO-UNDO. 
DEF VAR liFiles    AS INT  NO-UNDO.

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD AuthFile AS CHAR
   FIELD FileIdx  AS INT
   INDEX AuthFile AuthFile.

DEF STREAM sRead.

FUNCTION fCollTemp RETURNS LOGIC
   (icAuthFile AS CHAR,
    iiIndex    AS INT).

   /* file not found */
   IF icAuthFile = "" OR SEARCH(icAuthFile) = ? THEN NEXT.
   
   IF CAN-FIND(FIRST ttFiles WHERE ttFiles.AuthFile = icAuthFile) THEN NEXT.
   
   CREATE ttFiles.
   ASSIGN ttFiles.AuthFile = icAuthFile
          ttFiles.FileIdx  = iiIndex.
   
END FUNCTION.

lcInFile  = fCparamC("DDebitAuthFile").

/* payment configuration not done */
IF lcInFile = "" THEN RETURN.

FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Company THEN ynimi = Company.CompName.

fELog("DDAUTH","Started").

/* have wild-cards been used */
IF INDEX(lcInFile,"*") > 0 OR
   INDEX(lcInFile,"?") > 0
THEN DO:
   
   RUN pFindFiles (lcInFile,
                   OUTPUT lcAuthFile).
                      
   IF lcAuthFile > "" THEN 
   DO liCount = 1 TO NUM-ENTRIES(lcAuthFile,"¤"):
      fCollTemp(ENTRY(liCount,lcAuthFile,"¤"),
                liCount).
   END.
END.

/* single file */
ELSE DO:
   fCollTemp(lcInFile,
             1).  
END.

       
FOR EACH ttFiles:

   liFiles = liFiles + 1.

   /* booking list */
   oso  = "svval" + STRING(ttFiles.FileIdx) + "_"     +
          STRING(YEAR(TODAY),"9999") +
          STRING(MONTH(TODAY),"99")  +
          STRING(DAY(TODAY),"99")    +
          "_" + STRING(TIME) + ".txt".
   
   OUTPUT STREAM tul TO VALUE(oso).

   /* make sure that normal print routine is not used */
   str1 = "". 

   /* page length */
   ASSIGN spit1  = 80
          skayt1 = 80.

   RUN Ar/ddauthin (ttFiles.AuthFile, 
                 FALSE,      /* show messages */
                 TRUE,       /* send mail     */
                 OUTPUT liCount).
    
   /* delete or move the payment file to archive
      if no payments found from file -> move file anyway */
   RUN pDDAuthTrans(ttFiles.AuthFile). 
   
END.

fELog("DDAUTH","Stopped:" + STRING(liFiles)).

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

