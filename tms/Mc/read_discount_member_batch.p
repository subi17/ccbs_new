/* ---------------------------------------------------------------------------
  MODULE .......: read_discount_member_batch.p
  FUNCTION .....: batch process for reading discount updates from a file
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 29.05.12
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "Cron".
       
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/timestamp.i}
{Func/ftransdir.i}

DEF VAR lcFile      AS CHAR NO-UNDO.
DEF VAR liRead      AS INT  NO-UNDO. 
DEF VAR liError     AS INT  NO-UNDO.
DEF VAR liFiles     AS INT  NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR liCnt       AS INT  NO-UNDO.

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD DPMemberFile AS CHAR
   INDEX DPMemberFile DPMemberFile.

DEF STREAM sRead.

FUNCTION fCollTemp RETURNS LOGIC
   (icDPMemberFile AS CHAR).

   /* file not found */
   IF icDPMemberFile = "" OR SEARCH(icDPMemberFile) = ? THEN NEXT.
   
   IF CAN-FIND(FIRST ttFiles WHERE ttFiles.DPMemberFile = icDPMemberFile) THEN
      NEXT.
   
   CREATE ttFiles.
   ASSIGN ttFiles.DPMemberFile = icDPMemberFile.
   
END FUNCTION.


/******** Main start ******/

FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Company THEN ynimi = Company.CompName.

lcFile = fCParamC("ReadDPMemberFile").
   
IF lcFile = "" OR lcFile = ? THEN RETURN "ERROR:Definitions missing".
                                       
fELog("READ_DPMember","started").

RUN pFindFiles (lcFile,
                OUTPUT lcFile).
                      
IF lcFile > "" THEN 
DO liCnt = 1 TO NUM-ENTRIES(lcFile,"¤"):
   fCollTemp(ENTRY(liCnt,lcFile,"¤")).
END.

      
FOR EACH ttFiles:

   liFiles = liFiles + 1.
   
   RUN Mc/read_discount_member.p (ttFiles.DPMemberFile,
                               OUTPUT liRead,
                               OUTPUT liError).
   
END.

fELog("READ_DPMember","stopped,Files:" + STRING(liFiles)).

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
      
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.
      
      ocFiles = ocFiles + 
                (IF ocFiles > "" THEN "¤" ELSE "") + 
                lcFileName.
   END.
                        
   INPUT STREAM sRead CLOSE.

END PROCEDURE.


