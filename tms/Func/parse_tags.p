/* ----------------------------------------------------------------------
MODULE .......: parse_email_txt.p
TASK .........: Parses txt file and replace #[TAG] values
APPLICATION ..: TMS
AUTHOR .......: ilkkasav & kariaika 
CREATED ......: 19.3.2015
CHANGED ......:
Version ......: Yoigo
----------------------------------------------------------------------- */
&GLOBAL-DEFINE MailTitleSpaces Allow

DEFINE VARIABLE lcOriginalNumericFormat AS CHARACTER NO-UNDO. 
lcOriginalNumericFormat = SESSION:NUMERIC-FORMAT.
SESSION:NUMERIC-FORMAT = "European".

{commali.i}
{femaildata.i}
{email.i}
{ftransdir.i}
/*{femaildata2.i}*/

ASSIGN gcBrand = "1".

DEF INPUT PARAM icFile AS CHAR NO-UNDO.
DEF INPUT PARAM icOUTPUTFile AS CHAR NO-UNDO.
DEF INPUT PARAM iiOrderNbr AS INT NO-UNDO.
DEF INPUT PARAM iiemailSubj AS INT NO-UNDO.  /* 1 = conf req, 2 = conf */ 
DEF INPUT PARAM icEmailAddress AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocErr AS CHAR NO-UNDO.

DEF VAR lcCustEMail   AS CHAR NO-UNDO.
DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR lcEmailContent AS CHAR NO-UNDO.
DEF VAR llErrors AS LOG NO-UNDO.
DEF VAR liLanguage AS INT NO-UNDO.
DEF VAR lcTransDir AS CHAR NO-UNDO.
DEF VAR lcErrFile AS CHAR NO-UNDO.

DEF TEMP-TABLE wError NO-UNDO
    FIELD Onbr   AS INT
    FIELD ErrMsg AS CHAR.

DEF STREAM slog.

FILE-INFO:FILE-NAME = icFile.
IF FILE-INFO:FILE-TYPE = ? THEN DO:
   RETURN. /*appl_err("file not found: " + icFile).*/
END.

FUNCTION fisTag RETURNS LOGICAL (INPUT lcTag AS CHARACTER):
   DEF VAR lcTagName AS CHAR NO-UNDO.
   DEF VAR lcStart AS CHAR NO-UNDO.
   DEF VAR lcEnd AS CHAR NO-UNDO.

   lcStart = SUBSTRING(lcTag,1,1).
   lcEnd = SUBSTRING(lcTag,LENGTH(lcTag),1).
  /* Check that tags first and last character is capital letter.
      Otherwise not valid tag value */
   IF (ASC(lcStart) > 64 AND ASC(lcStart) < 91 AND 
       ASC(lcEnd) > 64 AND ASC(lcend) < 91) THEN RETURN TRUE.
   ELSE RETURN FALSE.
END FUNCTION.

/* Parce tags. Requirement tag starts with #[ and ends white ] */
FUNCTION fGetTag RETURNS CHAR (INPUT lcRow AS CHARACTER):
   DEF VAR lcTagName AS CHAR NO-UNDO.      
   DEF VAR liStart AS INT NO-UNDO.
   DEF VAR liEnd AS INT NO-UNDO.
   
   liStart = INDEX(lcRow,"#[").
   IF liStart > 0 THEN DO:
      liEnd = INDEX(lcRow,"]",liStart).   
      IF (liEnd > liStart) THEN DO: /* tag found */
         lcTagName =  SUBSTRING(lcRow,liStart + 2, liEnd - liStart - 2).
         DO WHILE (liStart <> 0 AND NOT fisTag(lcTagName)):
            liStart = INDEX(lcRow,"#[",liStart + 1).
            IF (liStart > 0) THEN DO:
               liEnd = INDEX(lcRow,"]",liStart).
               IF (liEnd > liStart) THEN 
                  lcTagName =  SUBSTRING(lcRow,liStart + 2, 
                                         liEnd - liStart - 2).
               ELSE DO:
                  lcTagName = "". /* no valid end mark found */
                  LEAVE.
               END.
            END.
            ELSE lcTagName = "". /* No valid tag found */
         END.
      END.   
      ELSE lcTagName = "". /* No valid end mark found */  
   END.
   RETURN lcTagName.
END FUNCTION.

FUNCTION fErrLine RETURNS LOGICAL
    (iMessage AS CHAR).

    CREATE wError.
    ASSIGN wError.Onbr   = iiOrdernbr
           wError.ErrMsg = iMessage.

END FUNCTION.

DEF STREAM sinfile.
INPUT STREAM sinfile FROM VALUE(icFile).

DEF VAR lcTag AS CHAR NO-UNDO.
DEF VAR lcFuncName AS CHAR NO-UNDO.
DEF VAR lcReplacementValue AS CHAR NO-UNDO.
DEF VAR liNotTagCount AS INT NO-UNDO.

DEF STREAM soutfile.
OUTPUT STREAM soutfile to VALUE(icOUTPUTFile).

repeat:
   IMPORT STREAM sinfile UNFORMATTED lcLine.
   liNotTagCount = 0. /* initialize to zero. Needed for possible # marks in text
                    that actually are not tags. */
   DO WHILE (INDEX(lcLine,"#[")>0 AND liNotTagCount = 0):
      lcTag = fGetTag(lcLine).
      IF (lcTag > "") THEN DO:
         lcFuncName = "pGet" + lcTag.
         RUN VALUE(lcFuncName)
             (INPUT iiOrdernbr, OUTPUT llErrors, OUTPUT lcReplacementValue).
         IF (NOT llErrors AND LENGTH(lcReplacementValue)>0) THEN
            lcLine = REPLACE (lcLine,"#[" + lcTag + "]",lcReplacementValue).
         ELSE DO:
            IF llErrors THEN fErrLine(INPUT lcReplacementValue).
            lcLine = "". /* Clear whole line because error or no data */
         END.   
      END.
      ELSE DO:
         liNotTagCount = liNotTagCount + 1.
      END.
   END.
   IF (lcLine > "") THEN DO:
      PUT STREAM soutfile UNFORMATTED lcLine skip.
      lcEmailContent = lcEmailContent + " " + lcLine.
   END.
  
END.

OUTPUT STREAM soutfile CLOSE.
INPUT STREAM sinfile CLOSE.

FIND FIRST OrderCustomer NO-LOCK WHERE
           OrderCustomer.Brand   = gcBrand     AND
           OrderCustomer.OrderID = iiOrderNbr  AND
           OrderCustomer.RowType = 1 NO-ERROR.

IF NOT llErrors AND OrderCustomer.Email  > ""
           AND NOT OrderCustomer.Email  BEGINS "@" THEN DO:

   ASSIGN
      xMailFrom = fCParamC("DefEmailSender")
      xMailAddr = icEmailAddress
      liLanguage = 1. /* INT(OrderCustomer.Language). only Spain decided to
                         be supported at the moment. YTS-7046 */
      IF liLanguage = 1 THEN DO:
         IF iiemailSubj = 1 THEN
            xMailSubj = "Confirmación de solicitud de Yoigo".
         ELSE xMailSubj = "Confirmación de pedido de Yoigo".
      END.   
      ELSE DO:
         IF iiemailSubj = 1 THEN
            xMailSubj = "Yoigo Order Confirmation Request".
         ELSE xMailSubj = "Yoigo Order Confirmation".
      END.
   /* Send the email */
      SendMaileInvoice("", "", icOUTPUTFile).
END.
ELSE DO:
   IF lcErrFile = "" THEN lcErrFile = "/tmp/prtxt".

   ASSIGN lcErrFile  = lcErrFile + "_" +
                               STRING(YEAR(TODAY),"9999") +
                               STRING(MONTH(TODAY),"99")  +
                               STRING(DAY(TODAY),"99")    +
                               /*"_" + STRING(TIME) +*/ ".txt".

   OUTPUT STREAM slog TO VALUE(lcErrFile) APPEND.
   PUT STREAM slog UNFORMATTED
       STRING(TODAY) + "_" + STRING(TIME) + "|" +
       OrderCustomer.Email + "|" +
       STRING(wError.onbr) + "|" + 
       wError.ErrMsg.

   OUTPUT STREAM slog CLOSE.

END.
   /* move the file to archive directory */
   lcTransDir = fCParam("Printing","MailArcDir").
   IF lcTransDir > "" THEN
      fTransDir(icOUTPUTFile,
                ".html",
                lcTransDir).

FINALLY:
   SESSION:NUMERIC-FORMAT = lcOriginalNumericFormat.
END.
