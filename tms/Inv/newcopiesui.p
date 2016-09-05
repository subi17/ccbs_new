{Syst/testpaa.i}
{Func/cparam2.i}

DEFINE VARIABLE lcInvoiceStatusDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInvoiceStatisticsDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInvoiceDir           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTmpDir               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMSISDNDir            AS CHARACTER NO-UNDO.
DEFINE VARIABLE ok                     AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE lcMSISDNFile           AS CHARACTER FORMAT "X(50)" NO-UNDO. 
DEFINE VARIABLE cKeyLabel              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lKeybFunction          AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE lFunctionKey           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iCountMonths           AS INTEGER   NO-UNDO. 
DEFINE VARIABLE iCountCLI              AS INTEGER   NO-UNDO. 
DEFINE VARIABLE iCountCombinations     AS INTEGER   NO-UNDO. 




FUNCTION fExistFile RETURN LOGICAL (INPUT pcFileName AS CHARACTER):
   IF SEARCH(pcFileName) <> ? THEN RETURN TRUE. ELSE RETURN FALSE.
END.

FUNCTION fCheckTMSParam RETURN LOGICAL 
   (OUTPUT pcoParamValue AS CHARACTER, INPUT pcParamName AS CHARACTER):

   pcoParamValue = fCParam( "WholeInvDir", pcParamName  ).
   IF pcoParamValue = ? THEN RETURN FALSE. ELSE RETURN TRUE.
END.

katun = "eka".

IF NOT fCheckTMSParam(OUTPUT lcInvoiceStatusDir    , "invoicestates") 
   THEN RETURN.
IF NOT fCheckTMSParam(OUTPUT lcInvoiceStatisticsDir, "invoicestatistics") 
   THEN RETURN.
IF NOT fCheckTMSParam(OUTPUT lcInvoiceDir          , "invoices") 
   THEN RETURN.
IF NOT fCheckTMSParam(OUTPUT lcTmpDir              , "tmp") 
   THEN RETURN.
IF NOT fCheckTMSParam(OUTPUT lcMSISDNDir           , "MSISDNListFileDir") 
   THEN RETURN.


ASSIGN iCountMonths = 0
       iCountCLI = 0 
       iCountCombinations = 0.

form
   skip(1)
   " Note:  This program produces invoices based on a file that describes " skip
   "        the months and CLI values." skip
   skip(1)
   "        File of months and CLI values:" SKIP(1)
   "       " lcMSISDNFile
   help "Choose existing file for months and CLI values" 
   skip(1)
   "        Number of months       : " iCountMonths        skip
   "        Number of CLI values   : " iCountCLI           skip
   "        Number of combinations : " iCountCombinations  skip(1)

WITH
   centered overlay no-labels title 
   " Produce invoice copy files based on a list " 
   ROW 2 FRAME MAIN.

MainLoop:
REPEAT WITH FRAME Main:
   ASSIGN ufk = 0
          ufk[1] = 7
          ufk[4] = 9812
          ufk[5] = 9813
          ufk[8] = 8
          ehto = 3.
   RUN Syst/ufkey.p.
   UPDATE lcMSISDNFile GO-ON(F1 F4 F5)
   WITH FRAME MAIN EDITING:
      READKEY.
      cKeyLabel = KEYLABEL(LASTKEY).
      lKeybFunction = FALSE.
      IF LENGTH(cKeyLabel) = 2 THEN
      DO:
         cKeyLabel = CAPS(cKeyLabel).
         lKeybFunction = TRUE.
      END.
      IF NOT lKeybFunction AND LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
      DO:
         APPLY LASTKEY.
      END.
      ELSE IF lKeybFunction THEN
      DO:
          IF cKeyLabel = "F1" THEN
          DO:
            toimi = 1.
            lFunctionKey = TRUE.
          END.
          ELSE IF cKeyLabel = "F4" THEN
          DO:
             toimi = 4.
             lFunctionKey = TRUE.
          END.
          ELSE IF cKeyLabel = "F5" THEN
          DO:
             toimi = 5.
             lFunctionKey = TRUE.
          END.
          ELSE IF cKeyLabel = "F8" THEN
          DO:
             LEAVE MainLoop.
          END.
          APPLY LASTKEY.
          lFunctionKey = FALSE.
      END.
      ELSE IF LOOKUP(KEYLABEL(LASTKEY), poisnap) > 0 THEN
      DO WITH FRAME Main:
         PAUSE 0.
         IF FRAME-FIELD = "lcMSISDNFile" AND lcMSISDNFile <> "" THEN
         DO:
            MESSAGE "lcMSISDNFile" VIEW-AS ALERT-BOX.
            DEFINE VARIABLE origMSISDNFile AS CHARACTER NO-UNDO. 
            IF NOT fExistFile( lcMSISDNDir + "/" + origMSISDNFile) THEN 
               lcMSISDNFile = "".
            origMSISDNFile = lcMSISDNFile.
            IF lcMSISDNFile <> origMSISDNFile THEN
               DISP lcMSISDNFile WITH FRAME Main.
         END.
         READKEY.
         APPLY LASTKEY.
      END.
   END. /* EDITING */
   ACTION:
   REPEAT WITH FRAME Main:
      IF toimi <> 0 THEN
      DO:
         DEFINE VARIABLE lLeaveAction AS LOGICAL NO-UNDO. 
         DEFINE VARIABLE lLeaveProgram AS LOGICAL NO-UNDO. 
         DEFINE VARIABLE lNextMain AS LOGICAL NO-UNDO. 
         RUN pDoAction(OUTPUT lLeaveAction, OUTPUT lNextMain, 
                       OUTPUT lLeaveProgram).
         IF lLeaveAction THEN LEAVE Action.
         IF lNextMain THEN NEXT MainLoop.
         IF lLeaveProgram THEN
         DO:
            LEAVE MainLoop.
         END.
      END.
      APPLY LASTKEY.
   END. /* Action */

   RUN Inv/newcopies2.p(
       lcMSISDNDir, lcInvoiceStatusDir, lcInvoiceStatisticsDir,
       lcTmpDir, lcInvoiceDir,
       lcMSISDNFile, 
       toimi = 5,
       OUTPUT iCountCombinations, OUTPUT iCountCLI, OUTPUT iCountMonths ).
   DISP iCountCombinations iCountCLI iCountMonths WITH FRAME Main.

END. /* MAIN */

DEFINE STREAM sFiles.
PROCEDURE pDoAction:
   DEFINE OUTPUT PARAMETER plLeaveAction AS LOGICAL NO-UNDO. 
   DEFINE OUTPUT PARAMETER plNextMain AS LOGICAL NO-UNDO. 
   DEFINE OUTPUT PARAMETER plLeaveProgram AS LOGICAL NO-UNDO. 

   ASSIGN 
     plLeaveAction = FALSE
     plNextMain = FALSE
     plLeaveProgram = FALSE.
   IF toimi = 1 THEN
   DO:
      DEFINE VARIABLE iCountFiles AS INTEGER NO-UNDO. 
      DEFINE VARIABLE cFile AS CHARACTER NO-UNDO. 
      iCountFiles = -1.
      INPUT STREAM sFiles THROUGH VALUE("ls -l " + lcMSISDNDir).
      REPEAT:
         IMPORT STREAM sFiles UNFORMATTED cFile.
         iCountFiles = iCountFiles + 1.
      END.
      INPUT STREAM sFiles CLOSE.

      IF iCountFiles > 0 THEN
      DO:
         RUN Syst/filebrowser.p(lcMSISDNDir).
         lcMSISDNFile = RETURN-VALUE.
         DISP lcMSISDNFile WITH FRAME Main.
         plNextMain = TRUE.
         RETURN.
      END.
      ELSE
      DO:         
         MESSAGE "Cannot use file selection, because" SKIP
                 "directory " lcMSISDNDir " does not contain any file."
                 VIEW-AS ALERT-BOX.
         plNextMain = TRUE.
         RETURN.
      END.
   END.

   IF toimi = 8 THEN
   DO:
      plLeaveProgram = TRUE.
      RETURN.
   END.

   IF toimi = 5 OR toimi = 4 THEN
   DO:
      IF lcMsisdnFile = "" THEN
      DO:
         MESSAGE "Choose file to describe months and CLI values first with F1"
         VIEW-AS ALERT-BOX.
         plNextMain = TRUE.
         RETURN.
      END.
      ELSE IF NOT fExistFile(lcMSISDNDir + "/" + lcMSISDNFile) THEN
      DO:
         MESSAGE "File " lcMSISDNFile 
             " does not exist. Choose existing file and try again." 
             VIEW-AS ALERT-BOX.
         plNextMain = TRUE.
         RETURN.
      END.
      ELSE
      DO:
         ok = FALSE.
         message "Do You REALLY want to start the load run (Y/N) ?" UPDATE ok.
         IF NOT ok THEN plNextMain = TRUE. ELSE plLeaveAction = TRUE.
         RETURN.
      END.
   END.
END.




