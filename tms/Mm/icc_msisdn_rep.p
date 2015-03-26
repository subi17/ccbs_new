/* ----------------------------------------------------------------------
  MODULE .......: icc_msisdn_rep.p
  TASK .........: Report SIM and MSISDN tables to the files and email 
                  taken from TMSParam.

                  This procedure is run from the cron. 
  APPLICATION ..: 
  AUTHOR .......: Harri Maenpaa (hm)
  CREATED ......: 
  CHANGED ......: 25.01.08 hm YOI-590
  Version ......: 
  ---------------------------------------------------------------------- */

{email.i}
{timestamp.i}
{commali.i}
{cparam2.i}
{msisdn.i}
{ftransdir.i}

OUTPUT THROUGH cat.

/**
 * This function checks if the given file can be opened for reading.
 *
 * @param pFileName  the filename of the checked file
 * @return TRUE, if the file can be opened for reading.
 */

FUNCTION fIsInputFile RETURNS LOGICAL (INPUT pFileName AS CHARACTER):

    /* Check if unexisting TMSParam */
    IF pFileName = ? OR pFileName = "" THEN 
    DO: 
       RETURN FALSE.
    END. /* If pFileName = ? ... */

    /* Check if the file exists: if it doesn't, it is not input file */
    IF SEARCH(pFileName) = ? THEN
       RETURN FALSE.

    /* Check if the target file is unreadable: file must have read rights */
    FILE-INFO:FILE-NAME = pFileName.
    IF INDEX(FILE-INFO:FILE-TYPE,"R") = 0 THEN
       RETURN FALSE.
   
    RETURN TRUE.
END.

/* Added validation to STATUSCODE 1 (ValidTo must be GT than fMakeTS() */


/* These flags indicate whether TMSParams can be found etc. 
   The first gets value TRUE if any error occurs.
   The second gets value TRUE if hardcoded error file cannot be opened,
   when the TMSParam error file is first tried. 
   lFirstErrorMsg indicates whether the error file is outputted first time.
*/
DEFINE VARIABLE lFail           AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lCriticalFail   AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFirstErrorMsg  AS LOGICAL NO-UNDO. 

/* Names for the files read and written: */
DEFINE VARIABLE lcErrorFile     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSimFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMSISDNFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcAddressFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPenMSISDNRep  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcPenSimRep     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcPenSpool      AS CHARACTER NO-UNDO. 

/* The error text to be written in the error file */
DEFINE TEMP-TABLE ttErrorText NO-UNDO
   FIELD ttText AS CHARACTER.

/* Strings to contain parts of the filenames when constructed to
   include timestamp */
DEFINE VARIABLE lcNamePart      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcExtPart       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTimeStampPart AS CHARACTER NO-UNDO.

ASSIGN lFail = FALSE
       lFirstErrorMsg = TRUE.

DEFINE STREAM sSIM.
DEFINE STREAM sMSISDN.

/* Get filenames from TMSParam */
ASSIGN
   lcErrorFile   = fcParam("Report", "ErrorFile")
   lcSimFile     = fcParam("Report", "SIMStatistics")
   lcMSISDNFile  = fcParam("Report", "MSISDNStatistics")
   lcAddressFile = fcParam("Report", "AddressFile")
   lcPenMSISDNRep = fCParam("Pentaho","PentahoMSISDN")
   lcPenSimRep    = fCParam("Pentaho","PentahoSIM")
   lcPenSpool     = fCParam("Pentaho","PentahoSpool").

/* File checking */

/* Adding timestamp to error file, SIM file and MSISDN file */
lcTimeStampPart = REPLACE(fUTCTime(0),":","_").  

RUN pSplitFileName(lcErrorFile, OUTPUT lcNamePart, OUTPUT lcExtPart).
lcErrorFile = lcNamePart + lcTimeStampPart + "." + lcExtPart.

RUN pSplitFileName(lcSimFile, OUTPUT lcNamePart, OUTPUT lcExtPart).
lcSimFile = lcNamePart + lcTimeStampPart + "." + lcExtPart.

RUN pSplitFileName(lcMSISDNFile, OUTPUT lcNamePart, OUTPUT lcExtPart).
lcMSISDNFile = lcNamePart + lcTimeStampPart + "." + lcExtPart.

/* Check error files */
IF NOT fIsOutputFile(lcErrorFile) THEN 
DO:
  DEFINE VARIABLE lcTMSParamErrorFile1 AS CHARACTER NO-UNDO. 
  lcTMSParamErrorFile1 = lcErrorFile.

  RUN pChangeToHardCodedErrorFile(lcTMSParamErrorFile1, lcTimeStampPart,
       OUTPUT lcErrorFile, OUTPUT lFail).
  lCriticalFail = lFail.
END. /* IF NOT IsOutputFile */


/* Check SIM and MSISDN files */   
IF NOT fIsOutputFile(lcSimFile) THEN
DO:
   RUN pReportFileError("Sim file", lcSimFile, "SIMStatistics", "created").
   lFail = TRUE.
END. /* IF NOT IsOutputFile(lcSimFile */


IF NOT fIsOutputFile(lcMSISDNFile) THEN
DO:
   RUN pReportFileError("MSISDN file", lcMSISDNFile, "MSISDNStatistics", 
      "created").
   lFail = TRUE.
END.  /* IF NOT IsOutputFile(lcMSISDNFile */

IF NOT fIsInputFile(lcAddressFile) THEN
DO:
   RUN pReportFileError("Address file", lcAddressFile, 
                       "AddressFile", "read").
   lFail = TRUE.
END. /* IF NOT IsOutputFile(lcAddressFile */


IF NOT lFail THEN
DO:
   GetRecipients(lcAddressFile).
   
   IF xMailAddr = "" THEN 
   DO:
      RUN pReportError("icc_msisdn_rep: " + 
         "AddressFile " + lcAddressFile + " was incorrect.").
      lFail = TRUE.
   END.
END.

/* Check Admin email */   
xMailAdmin = fcParam("Report", "AdminEmail").
IF xMailAdmin = ? OR xMailAdmin = "" THEN
DO:
   xMailAdmin = "sbtech@starnet.fi".
   RUN pReportError("icc_msisdn_rep: " + 
      "Admin mail address did not exist in TMSParam AdminEmail").
   lFail = TRUE.
END. /* IF xMailAdmin = ? ... */

/* Write error message to error file if any error occured.
   If lFail at least the hardcoded error file can be written.
   If lCriticalFail then not even hardcoded error file can
   be written. */
IF lFail AND NOT lCriticalFail THEN 
DO:
   RUN pProduceErrorFileAndSendMail.
END. /* IF lFail ... */

/* Return if any failure with files, ... */
IF lFail THEN RETURN.

/* Temp table to which MSISDN counts are stored before 
   outputting them to MSISDN statistics file */
DEFINE TEMP-TABLE ttMSISDN NO-UNDO
   FIELD Stock   AS CHARACTER
   FIELD Stat    AS INTEGER
   FIELD Num     AS INTEGER
   INDEX Stock AS PRIMARY
      Stock
      Stat.

/* Create reports */
RUN pSimReport.

RUN pMSISDNReport.

/**
 * This procedure is used to report error about the original error file,
 * and to produce the name for the hardcoded error file filename 
 * with time stamp and to test that it can be written. 
 * If there occurs error the error parameter is set TRUE, which
 * in the main procedure sets the CriticalError to TRUE. 
 *
 * @param pcOrigErrorFile   The original error file filename
 * @param pcTimeStampPart   The timestamp text to be added to the error file.
 * @param pcNewErrorFile    The hardcoded error file filename
 * @param plFail            Set TO TRUE if hardcoded error file cannot be
 *                          opened or appended.
 */
PROCEDURE pChangeToHardCodedErrorFile:
   DEFINE INPUT PARAMETER  pcOrigErrorFile AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER  pcTimeStampPart AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER pcNewErrorFile     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER plFail          AS LOGICAL   NO-UNDO.

   plFail = FALSE.

   /* ReportFileError uses lcErrorFile to output error messages
      if possible. The errors are also emailed to admin email. 
      Hardcoded error file is the last chance file operation
      for error operations. */

   lcErrorFile = "/tmp/icc_msisdn_rep_errorfile" + pcTimeStampPart + ".txt".
   pcNewErrorFile = lcErrorFile.

   RUN pReportFileError("Error file ", pcOrigErrorFile, "ErrorFile",
                  "created").
   IF NOT fIsOutputFile(lcErrorFile) THEN
   DO:
      plFail = TRUE.
   END. /* IF NOT IsOutputFile */
END.

/**
 * This procedure produces the report about number of the SIM records of 
 * each Stock (POS), Status and Manufacturer information. */
PROCEDURE pSimReport:
   OUTPUT STREAM sSIM TO VALUE(lcSimFile).
   
   FOR EACH SIM NO-LOCK WHERE
            SIM.Brand = "1"
   BREAK
      BY SIM.Stock
      BY SIM.ManCode
      BY SIM.SimArt
      BY SIM.SimStat:


      ACCUMULATE SimStat (SUB-COUNT BY Sim.Stock BY SIM.SimStat).

      IF LAST-OF(SIM.SimStat) THEN 
      DO:
         PUT STREAM sSIM UNFORMATTED
            STRING(TODAY) CHR(9)
            SIM.Stock   CHR(9)
            SIM.SimStat CHR(9)
         (ACCUM SUB-COUNT BY SIM.SimStat SIM.SimStat) CHR(9)
            SIM.ManCode CHR(9)
            SIM.SimArt  SKIP.
      END.

   END.
   OUTPUT STREAM sSIM CLOSE.
  
   /* copy and move pentaho copy */
   UNIX SILENT VALUE("cp " + lcSimFile + " " + lcPenSpool).
   DEFINE VARIABLE lcPenFileName AS CHARACTER NO-UNDO. 
   lcPenFileName =  fGetFileName(lcSimFile).
   UNIX SILENT VALUE("mv " + lcPenSpool + "/" + lcPenFileName + " " + lcPenSimRep).


END PROCEDURE.



/**
 * This procedure produces the report about number of the free (Status=1, POS empty) 
 * MSISDN records of each Stock (POS) and Status combination.
 * Uses temp table ttMSISDN to gather the counts first before writing 
 * them to the report file.
 */
PROCEDURE pMSISDNReport:
   FOR EACH MSISDN NO-LOCK USE-INDEX CLI 
      BREAK BY MSISDN.CLI:

      /* Only newest with same CLI is accepted; first is newest */   
      IF FIRST-OF(MSISDN.CLI) THEN
      DO:

         /* Only MSISDN with non empty POS are included */
         IF MSISDN.POS = "" THEN NEXT.

         FIND FIRST ttMSISDN WHERE
                    ttMSISDN.Stock = MSISDN.POS AND
                    ttMSISDN.Stat  = MSISDN.StatusCode
                    NO-ERROR.
   
         /* When ttMSISDN with POS and StatusCode is not found, 
            new record is created and count is 1 in the beginning.
            When ttMSISDN with POS and StatusCode is found,
            only count of the found ttMSISDN is incremented by 1 */
         IF NOT AVAIL ttMSISDN  THEN 
         DO:
            CREATE ttMSISDN.
            ASSIGN
               ttMSISDN.Stock   = MSISDN.POS
               ttMSISDN.Stat    = MSISDN.StatusCode.
         END. /* IF NOT AVAIL ttMSISDN */

         ttMSISDN.Num = ttMSISDN.Num + 1.

      END. /* IF FIRST-OF(MSISDN.CLI) */
   END. /* FOR EACH MSISDN */

   OUTPUT STREAM sMSISDN TO VALUE(lcMSISDNFile).
   FOR EACH ttMSISDN BREAK BY ttMSISDN.Stock:
      PUT STREAM sMSISDN UNFORMATTED
         STRING(TODAY)  CHR(9)
         ttMSISDN.Stock CHR(9)
         ttMSISDN.Stat  CHR(9)
         ttMSISDN.Num   SKIP.
   END. /* FOR EACH ttMSISDN */
   OUTPUT STREAM sMSISDN CLOSE.

   /* copy and move pentaho copy */
   UNIX SILENT VALUE("cp " + lcMSISDNFile + " " + lcPenSpool).
   DEFINE VARIABLE lcPenFileName AS CHARACTER NO-UNDO. 
   lcPenFileName =  fGetFileName(lcMSISDNFile).
   UNIX SILENT VALUE("mv " + lcPenSpool + "/" + lcPenFileName + " " + lcPenMSISDNRep).

END PROCEDURE.

/**
* This procedure produces a message about file operation error to
* error message. 
*
* @param cFileDesc the description of the file for which the 
*                  operation was performed.
* @param cFileName the name of the file for which operation is
*                  performed.
* @param cTMSParamName  the TMSParam from which the filename was
*                       taken.
* @param cActionDesc the operation performed for the file.
*/
PROCEDURE pReportFileError:
   DEFINE INPUT PARAMETER cFileDesc AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER cFileName AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER cTMSParamName AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER cActionDesc AS CHARACTER NO-UNDO. 

   IF cFileName = "" OR cFileName = ? THEN
      RUN pReportError( "icc_msisdn_rep: " + cFileDesc +
                   " did not exist in TMSParam " + cTMSParamName).
   ELSE
      RUN pReportError( "icc_msisdn_rep: " + cFileDesc +
         " " + cFileName + " cannot be " + cActionDesc).
END.

/**
 * Adds row to the error message string lcErrorText.
 * @param pMessage  the error message
 */
PROCEDURE pReportError:
   DEFINE INPUT PARAMETER pMessage AS CHARACTER NO-UNDO.

   CREATE ttErrorText.
   ASSIGN ttErrorText.ttText = pMessage.
END.

/**
 * Writes the error message string to the error message
 * file specified with lcErrorFile filename and
 * sends it to the Admin email address that is read
 * from TMSParam in the external procedure.
 */
DEFINE STREAM sError.
PROCEDURE pProduceErrorFileAndSendMail:
   IF lFirstErrorMsg THEN
   DO:
      IF fIsOutputFile(lcErrorFile) THEN
      DO:
         OUTPUT STREAM sError TO VALUE(lcErrorFile).
      END.
   END.
   ELSE 
   DO:
      IF fIsOutputFile(lcErrorFile) THEN
      DO:
         OUTPUT STREAM sError TO VALUE(lcErrorFile) APPEND.
      END.
   END.
  
   FOR EACH ttErrorText:
      PUT STREAM sError UNFORMATTED ttErrorText.ttText SKIP.
   END.
   OUTPUT STREAM sError CLOSE.
   
   xMailAddr = xMailAdmin.
   xMailSubj = "Error in icc_msisdn_rep generation".
   SendMail(lcErrorFile, "").

   lFirstErrorMsg = FALSE.
END.

/**
 * Splits given filename to the namepart and extension part.
 * Here the name part includes the directory path of the filename.
 * This procedure is used when adding timestamps in the end of
 * the name parts of the filenames.
 *
 * @param cFilename  the given filename
 * @param cNamePart  the extracted name part of the filename
 * @param cExtPart   the extracted extension part of the filename.
 */
PROCEDURE pSplitFileName:
   DEFINE INPUT PARAMETER cFileName AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER cNamePart AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER cExtPart AS CHARACTER NO-UNDO. 

   DEFINE VARIABLE iNumEntries AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iEntry AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cEntry AS CHARACTER NO-UNDO. 

   iNumEntries = NUM-ENTRIES(cFileName, ".").
   REPEAT iEntry = 1 TO iNumEntries - 1:
      cEntry = ENTRY(iEntry, cFileName, ".").
      IF iEntry = 1 THEN
         cNamePart = cEntry.
      ELSE
         cNamePart = cNamePart + "." + cEntry.
   END. /* REPEAT iEntry */
   cExtPart = ENTRY(iNumEntries, cFileName, ".").
END.
