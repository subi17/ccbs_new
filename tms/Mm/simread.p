/* ----------------------------------------------------------------------
  MODULE .......: SIMread.p
  TASK .........: Check OR read a SIM PaymFile.
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 03-06-99
  CHANGED ......: 29.06.99 pt isc1
                  14.10.02 jr Removed BillLevel
                  09.09.03 jp brand
                  12.02.04 jp default values for pin1 and pin2, if empty
                  16.03.07 kl do not move file here

  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/date.i}
{Func/ftmscode.i}

DEF INPUT PARAMETER SIMfile AS CHARACTER NO-UNDO. 
DEF INPUT PARAMETER ProcessedDir AS CHARACTER NO-UNDO. 
DEF OUTPUT PARAMETER ProcessedSIMFile AS CHARACTER NO-UNDO.  


DEF VAR SIMrow         AS c  NO-UNDO.
def var IMSI           as c  no-undo format "x(18)".
def var ICC            as c  no-undo format "x(20)".
def var PIN1           as c  no-undo format "x(6)".
def var PIN2           as c  no-undo format "x(4)".
DEF VAR PUK1           AS c  NO-UNDO.
DEF VAR PUK2           AS c  NO-UNDO.
def var KI             as c  no-undo format "x(12)".
def var ISC1           as c  no-undo format "x(4)".
DEF VAR phase          AS i  NO-UNDO INIT -1.
DEF VAR Stock          LIKE Stock.Stock   NO-UNDO.
DEF VAR SIMStat        LIKE SIMStat.SIMStat NO-UNDO.
DEF VAR cSIMStat       AS c  NO-UNDO FORMAT "X(1)".
DEF VAR tot            AS i  NO-UNDO.
DEF VAR d-icc          AS i  NO-UNDO.
DEF VAR d-IMSI         AS i  NO-UNDO.
DEF VAR i              AS i  NO-UNDO.
def var OK             as lo no-undo format "Yes/No".
DEF VAR iSimBatch         AS i  NO-UNDO. 
DEF VAR TpKey         LIKE SimBatch.TpKey.
DEF VAR DelDate        AS DA NO-UNDO init TODAY.
DEF VAR cSIMFileDir AS CHARACTER NO-UNDO. 
DEF VAR cSIMFileNamePart AS CHARACTER NO-UNDO. 
DEF VAR cSIMFileNamePartNoExt AS CHARACTER NO-UNDO. 
DEF VAR cSIMFileExtPart AS CHARACTER NO-UNDO. 
DEF VAR iColon AS INTEGER NO-UNDO. 
DEF VAR sTPValue AS CHARACTER NO-UNDO.
DEF VAR llSerNbFound AS LOG  NO-UNDO.
DEF VAR lcManCode    AS CHAR NO-UNDO.
DEF VAR lcSimArt     AS CHAR NO-UNDO.

DEF STREAM SIM.

FUNCTION fOnlyDigits RETURN LOGICAL (INPUT pcInput AS CHARACTER):
   DEFINE VARIABLE iLength AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iChar AS INTEGER NO-UNDO. 
   CharLoop:
   REPEAT iChar = 1 TO iLength:
      IF SUBSTRING(pcInput, iChar, 1) < "0" OR
         SUBSTRING(pcInput, iChar, 1) > "9" THEN RETURN FALSE.
   END.
   RETURN TRUE.
END.


form
   skip(1)
   " Note:  This program loads a new set of SIM/IMSI data from a vendor's " skip
   "        File into SIM/IMSI records in the billing database." skip(1)
   "        All new SIMS are also placed into stock and status named below."   
   skip(1)
   "        Stock Code ....:" Stock FORMAT "x(12)" no-label
      help "Code Of Stock Location" 
   Stock.StoName   at 42  format "x(30)"           SKIP
   "        SIM Status Code:" cSimStat 
            VALIDATE( fOnlyDigits( INPUT cSimStat ), "Enter only digits")
      help "Common Status Code for all new SIMs" 
   SIMStat.SSName at 42 format "x(30)" 
   skip(1)

   "        File name ......:" cSIMFileNamePart FORMAT "x(30)" skip
   "        Records read ...:" tot                                         skip
   "        Duplicate SIM ..:" d-icc                                       skip
   "        Duplicate IMSI .:" d-IMSI                                      skip
   skip(1)

WITH
   centered overlay no-labels title " Load New SIM/IMSI Data " 
   ROW 2 FRAME MAIN.

/* set the status code FOR a NEW SIM card */
{Func/tmsparam.i SIMStatusNew return}. SIMStat = TMSParam.IntVal. 
cSimStat = STRING(SIMStat).
{Func/tmsparam.i MainStock    return}. Stock = TMSParam.CharVal.

DEFINE STREAM sIFile.
FUNCTION IsInputFile RETURNS LOGICAL (INPUT pFileName AS CHARACTER):
    IF pFileName = ? OR pFileName = "" THEN 
    DO: 
       RETURN FALSE.
    END. /* If pFileName = ? ... */
    
    DO ON ERROR UNDO, RETURN FALSE:
       INPUT STREAM sIFile FROM VALUE(pFileName).
       DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
       REPEAT:
          IMPORT STREAM sIFile UNFORMATTED lcLine.
       END. /* REPEAT IMPORT STREAM sIFile */
       INPUT STREAM sIFile CLOSE.
    END. /* DO ON ERROR */
    RETURN TRUE.
END.


/* read the description record from default */
FIND IFiSpx where 
     IFiSpx.Brand   = gcBrand   AND
     IFiSpx.ManCode = "GEMALTO" AND
     IFiSpx.Version = "00001" NO-LOCK.

PAUSE 0.

FIND Stock WHERE 
     Stock.Brand = gcBrand AND 
     Stock.Stock = Stock
no-lock no-error.

IF NOT AVAIL Stock THEN DO:
   MESSAGE
      "Stock" Stock "is defined as a default Stock" SKIP
      "in System Parameter File But it is not found"  SKIP
      "in the Stock File"
   view-as alert-box error title " LOAD ABORTED ".
   RETURN.
END.

SIMStat = INTEGER(cSIMStat) NO-ERROR.

FIND SIMStat WHERE
   SIMStat.SIMStat = SIMStat
   no-lock no-error.

IF NOT AVAIL SIMStat THEN 
DO:
   MESSAGE
      "Status Code"  SIMStat "is defined as a default code" SKIP
      "in System Parameter File But it is not found"  SKIP
      "in the SIM Status Code File"
      view-as alert-box error title " LOAD ABORTED ".
   RETURN.
END.


IF SEARCH(SIMfile) = ? THEN DO:
  MESSAGE 
     "The vendor's SIM Data File" SKIP
     SIMfile SKIP
     "is not found"
     VIEW-AS ALERT-BOX ERROR TITLE " LOAD ABORTED ".
  RETURN.
END.  

DEFINE VARIABLE lcErrorMsg AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lError AS LOGICAL NO-UNDO. 
lError = FALSE.

DEFINE VARIABLE lTPFound AS LOGICAL NO-UNDO. 
lTPFound = FALSE.

RUN SplitFilePath(SIMfile, 
   OUTPUT cSIMFileDir,
   OUTPUT cSIMFileNamePart, 
   OUTPUT cSIMFileNamePartNoExt,
   OUTPUT cSIMFileExtPart).

ProcessedSIMFile = ProcessedDir + cSimFileNamePart.

view FRAME main.
DISP 
   cSimFileNamePart
   Stock
   Stock.StoName 
   cSIMStat
   SIMStat.SSName
WITH  FRAME main.

/* Read TP header value */
INPUT STREAM SIM FROM VALUE(SIMFILE).

hrows:
DO i = 1 TO IFiSpx.Hrowd:    
   IMPORT STREAM SIM UNFORMATTED SIMrow.  

   IF INDEX(SIMrow, "Transport_key") = 1 THEN
   DO:
      iColon = INDEX(SIMrow, ":").
      sTPValue = SUBSTRING(SIMRow, iColon + 1).
      IF NOT sTPValue = "" THEN
      DO:
         DEFINE VARIABLE iValue AS INTEGER NO-UNDO. 
         iValue = INTEGER(sTPValue) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            TPKey = "1".
         ELSE
            TPKey = STRING(iValue).
         lTPFound = TRUE.
      END.
   END.

   /* Parse SIM Provider and Type */
   IF INDEX(SIMrow, "Ser_nb") = 1 THEN
   DO:
      ASSIGN iColon       = INDEX(SIMrow, ":")
             sTPValue     = SUBSTRING(SIMRow, iColon + 1)
             sTPValue     = TRIM(sTPValue," ")
             sTPValue     = TRIM(sTPValue,CHR(9))
             llSerNbFound = TRUE.

      IF sTPValue = "" THEN
         llSerNbFound = FALSE.
      ELSE DO:
         lcManCode = fTMSCodeConfigValue("IFiSpx","ManCode",
                                         SUBSTRING(sTPValue,7,1)).
         lcSimArt  = fTMSCodeConfigValue("IFiSpx","SimArt",
                                         SUBSTRING(sTPValue,7,2)).
         /* Find actual record */
         FIND FIRST IFiSpx WHERE
                    IFiSpx.Brand   = gcBrand   AND
                    IFiSpx.ManCode = lcManCode AND
                    IFiSpx.SimArt  = lcSimArt NO-LOCK NO-ERROR.
         IF NOT AVAIL IFiSpx THEN llSerNbFound = FALSE.
      END.
      IF llSerNbFound = FALSE THEN LEAVE hrows.
   END.

   IF lTPFound AND llSerNbFound THEN LEAVE hrows.
END.   

INPUT STREAM SIM CLOSE.

IF NOT llSerNbFound THEN DO:
   MESSAGE 
      "Invalid Serial Number specified in the SIM Data File: " sTPValue
      VIEW-AS ALERT-BOX ERROR TITLE " LOAD ABORTED ".
   RETURN.
END. /* IF NOT llSerNbFound THEN DO: */

IF NOT lTPFound THEN TPKey = "1".

/* This variable defines whether file will be moved to 
   the processed directory */
DEFINE VARIABLE lRealUpdate AS LOGICAL NO-UNDO. 
lRealUpdate = FALSE.

MAIN:
REPEAT WITH FRAME Main:
  
   IF phase NE -1 THEN
   UPDATE_LOOP:
   REPEAT ON ENDKEY UNDO, LEAVE:
      
      ehto = 9. RUN Syst/ufkey.p.
      
      UPDATE 
         Stock
      WITH FRAME MAIN EDITING:
         READKEY.
         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
         DO WITH FRAME main:
            PAUSE 0.
            IF FRAME-FIELD = "Stock" THEN 
            DO:
               FIND Stock WHERE 
                    Stock.Brand = gcBrand AND 
                    Stock.Stock =
               INPUT FRAME main Stock NO-LOCK NO-ERROR.
               IF NOT AVAIL Stock THEN 
               DO:
                  BELL.
                  MESSAGE "Unknown Stock !".
                  NEXT.
               END.
               DISP Stock.StoName WITH FRAME main.
            END.

            ELSE IF FRAME-FIELD = "cSIMStat" THEN 
            DO:
               DEFINE VARIABLE iSimStat AS INTEGER NO-UNDO. 
               iSimStat = INTEGER(cSimStat) NO-ERROR.
               IF NOT ERROR-STATUS:ERROR THEN
               DO:
                  FIND SIMStat WHERE SIMStat.SIMStat =
                  INTEGER(INPUT FRAME main cSIMStat) NO-LOCK NO-ERROR.
                  IF NOT AVAIL SIMStat THEN 
                  DO:
                     BELL.
                     MESSAGE "Unknown SIM Status Code !".
                     NEXT.
                  END.
                  DISP SIMStat.SSName WITH FRAME main.
               END.
               ELSE
                  MESSAGE "Enter only digits for Status code" VIEW-AS ALERT-BOX.
            END.
         END.
         APPLY LASTKEY.
      END. /* EDITING */

      LEAVE UPDATE_LOOP.
   END.
   
   DISP 
      Stock
      Stock.StoName 
      cSIMStat
      SIMStat.SSName
   WITH  FRAME main.
   
   phase = 0.

   ACTION:
   REPEAT WITH FRAME Main:
      ASSIGN
      ufk =  0 ufk[1] = 7 ufk[4] = 241
      ufk[5] = 795 ufk[8] = 8 ehto = 0.  RUN Syst/ufkey.p.
      IF toimi = 1 THEN NEXT  main.
      IF toimi = 8 THEN LEAVE main.
      IF toimi = 4 THEN LEAVE Action.
      IF toimi = 5 THEN 
      DO:
         ok = FALSE.
         message "Do You REALLY want to start the load run (Y/N) ?" UPDATE ok.
         IF NOT ok THEN NEXT Action.
         LEAVE Action.
      END.
   END. /* Action */

   ufk =  0. ehto = 3. RUN Syst/ufkey.p.

   lError = FALSE.

   DEFINE VARIABLE lcICCBegin AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcIMSIBegin AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liRowLength AS INTEGER NO-UNDO. 
  
   DEFINE VARIABLE lcRowLengthParamCode AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcRowLength AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE iRow AS INTEGER NO-UNDO.

   lcRowLengthParamCode = "ICC_" + IFiSpx.ManCode + "_" + IFiSpx.Version.
   lcRowLength = fcParam( "SIM", lcRowLengthParamCode ).
   IF lcRowLength = ? THEN
   DO:
      MESSAGE "Expected row length parameter " lcRowLengthParamCode 
         " was not found from the database." VIEW-AS ALERT-BOX.
      NEXT Main.
   END.
   liRowLength = INTEGER( lcRowLength) NO-ERROR.
   if ERROR-STATUS:ERROR THEN
   DO:
      MESSAGE "Excected Row length parameter " lcRowLengthParamCode
      " was illegal in the database." VIEW-AS ALERT-BOX.
      NEXT Main.
   END.
  
   lcICCBegin  = fCParam( "SIM", "ICC_Begin"  ).
   lcIMSIBegin = fCParam( "SIM", "IMSI_Begin" ).
   lcErrorMsg = "Incoming file " + SIMfile + " is incorrect. Check file structure.".

   RUN HandleUniqueSIMFileName(INPUT-OUTPUT cSIMFileNamePart,
       ProcessedDir, cSIMFileNamePartNoExt, cSIMFileExtPart).

   PhaseLoop:
   DO  phase = 1 TO (IF toimi = 4 THEN 1 ELSE 2):    /* 1: preceeding check;  
                                                        2: UPDATE */
      IF phase = 2 THEN 
      DO:
         /*******************************
         * Were there any errors during *
         * the preceeding check ?       *
         ********************************/

         /* Duplicate ICCs ? */
         IF d-icc > 0 THEN 
            MESSAGE "Totally" tot "SIM records were read." SKIP
               d-icc "ICC codes were found already in"  SKIP
               "the SIM database.  Load shall be aborted."
               VIEW-AS ALERT-BOX error.

         /* Duplicate IMSIs ? */
         IF d-IMSI > 0 THEN 
            MESSAGE "Totally" tot "SIM records were read." SKIP
               d-IMSI "IMSI codes were found already in"  SKIP
               "the IMSI database.  Load shall be aborted."
               VIEW-AS ALERT-BOX error.
      
         IF lError THEN 
            MESSAGE lcErrorMsg VIEW-AS ALERT-BOX BUTTONS OK.

         IF d-icc + d-IMSI > 0 THEN RETURN.
         IF lError THEN RETURN.
         
      END.
         
      ASSIGN
         tot    =  0
         d-icc  = 0
         d-IMSI = 0.

      /* open the SIM PaymFile */
      INPUT STREAM SIM from value(SIMfile).

      /* SKIP header rows */
      DO i = 1 TO IFiSpx.Hrowd:    
         IMPORT STREAM SIM UNFORMATTED SIMrow.  
      END.   
      iRow = IFiSpx.hRowd.

      SIMFILE_TRANS:
      DO TRANSACTION:
      readSIMrows:
      repeat:
         SIMrow = "".
         IMPORT STREAM SIM UNFORMATTED SIMrow.
         iRow = iRow + 1.

         /* Empty rows are skipped */
         if SIMrow = "" THEN NEXT.

         /* Check the row length */
         DEFINE VARIABLE iRowLength AS INTEGER NO-UNDO. 
         iRowLength = LENGTH(SIMRow).

         IF NOT iRowLength =  liRowLength THEN /* End of line is stripped when row is read */
         DO:
            lError = TRUE.
            lcErrorMSg = lcErrorMsg + " The row length was incorrect at row " 
                         + STRING(iRow) + ".".
            LEAVE readSIMrows.
         END.

         /* If first character of the row is not a digit, skip the line */
         if not index("0123456789",substr(SIMrow,1,1))  > 0 THEN NEXT.

         /* Parse different fields and check them */

         /* IMSI */
         IMSI = substr(SIMrow,IFiSpx.IMSI [1],IFiSpx.IMSI [2]).
         IF NOT IMSI BEGINS lcIMSIBegin OR INDEX(IMSI, " ") > 0 THEN
         DO:
            lError = TRUE.
            lcErrorMsg = lcErrorMsg + " IMSI field was incorrect at row "
                         + STRING(iRow).
            LEAVE readSIMrows.
         END.

         /* ICC */
         ICC  = substr(SIMrow,IFiSpx.ICC  [1],IFiSpx.ICC  [2]).
         IF NOT ICC BEGINS lcICCBegin OR INDEX(ICC, " ") > 0 THEN
         DO:
            lError = TRUE.
            lcErrorMsg = lcErrorMsg + " ICC field was incorrect at row "
                         + STRING(iRow).
            LEAVE readSIMrows.
         END.

         /* PIN1, PIN2, PUK1, PUK2 */
         ASSIGN 
            /* PIN 1 */
            PIN1 = substr(SIMrow,IFiSpx.IsCode1[1],IFiSpx.IsCode1[2])

            /* PIN 2 */
            PIN2 = substr(SIMrow,IFiSpx.IsCode2[1],IFiSpx.IsCode2[2])

            /* PUK 1 */
            PUK1 = substr(SIMrow,IFiSpx.IsUnb1 [1],IFiSpx.IsUnb1 [2])
  
            /* PUK 2 */
            PUK2 = substr(SIMrow,IFiSpx.IsUnb2 [1],IFiSpx.IsUnb2 [2]).

         IF INDEX(PIN1, " ") > 0 THEN
         DO:
            lError = TRUE.
            lcErrorMsg = lcErrorMsg + " PIN1 field contained spaces at row "
                         + STRING(iRow) + ".".
            LEAVE readSIMrows.            
         END.
         
         
         IF INDEX(PIN2, " ") > 0 THEN
         DO:   
            lError = TRUE.
            lcErrorMsg = lcErrorMsg + " PIN2 field contained spaces at row "
                         + STRING(iRow) + ".".
            LEAVE readSIMrows.
         END.

         IF INDEX(PUK1, " ") > 0 THEN
         DO:
            lError = TRUE.
            lcErrorMsg = lcErrorMsg + " PUK1 field contained spaces at row "
                         + STRING(iRow) + ".".
            LEAVE readSIMrows.
         END.

         IF INDEX(PUK2, " ") > 0 THEN
         DO:
            lError = TRUE.
            lcErrorMsg = lcErrorMsg + " PUK2 field contained spaces at row "  
                         + STRING(iRow) + ".".
            LEAVE readSIMrows.
         END.

         /* KI (NOT always ...) */
         IF IFiSpx.Ki[1] NE 0 AND IFiSpx.Ki[2] NE 0 THEN
         DO:
            KI   = substr(SIMrow,IFiSpx.Ki   [1],IFiSpx.Ki   [2]).
            IF INDEX(KI, " ") > 0 THEN
            DO:
               lError = TRUE.
               lcErrorMsg = lcErrorMSg + " KI field contained spaces at row " 
                            + STRING(iRow) + ".".
               LEAVE readSIMrows.
            END.
         END.

         /* ISC1 (NOT always ...) */
         IF IFiSpx.Isc1[1] NE 0 AND IFiSpx.Isc1[2] NE 0 THEN
         DO:
            ISC1  = substr(SIMrow,IFiSpx.Isc1 [1],IFiSpx.isc1 [2]).
            IF INDEX(ISC1, " ") > 0 THEN
            DO:
               lError = TRUE.
               lcErrorMsg = lcErrorMSg + " ISC1 field contained spaces at row "
                            + STRING(iRow) + ".".
               LEAVE readSIMrows.
            END.
         END.

         /*************************************
         * Check validity:  There shall  NOT  *
         * be any SIM WITH this ICC  neither  *
         * any    IMSI WITH this IMSI no.     *
         *************************************/
         tot = tot + 1.

         FIND SIM where 
            Sim.Brand = gcBrand AND 
            SIM.ICC   = ICC
            no-lock no-error.
         
         IF AVAIL SIM THEN d-icc = d-icc + 1.

         FIND IMSI where
            IMSI.IMSI = IMSI
            no-lock no-error.
         
         IF AVAIL IMSI THEN d-IMSI = d-IMSI + 1.

         PAUSE 0.
         DISP tot d-IMSI d-icc WITH FRAME main.

         IF phase =  1 THEN NEXT.

         /************************************
         * CREATE a NEW SimBatch IF any NEW  *
         * SIM is TO be created              *
         ************************************/
         IF iSimBatch = 0 THEN
         DO:
            CREATE SimBatch.
            ASSIGN
            SimBatch.SimBatch = NEXT-VALUE(simbatch)
            SimBatch.ManCode = IFiSpx.ManCode
            SimBatch.Brand  = gcBrand 
            SimBatch.SimArt = IFiSpx.SimArt
            SimBatch.DelDate = TODAY
            SimBatch.TpKey  = TpKey
            SimBatch.FileName = cSIMFileNamePart. 
            iSimBatch    = SimBatch.SimBatch.
         END.

         /*************************************
         * CREATE now an individual SIM card  *
         * record WITH                        *
         *************************************/

         CREATE SIM.
         ASSIGN
            SIM.Brand       = gcBrand 
            SIM.SimBatch    = isimbatch
            SIM.ManCode     = IFiSpx.ManCode
            SIM.ICC         = icc
            SIM.ISC1        = isc1
            SIM.Stock       = Stock
            SIM.SIMStat     = SimStat
            SIM.SimArt      = IFiSpx.SimArt
            SIM.CustNum     =  0.

         /**************************
         * CREATE the IMSI record  *
         **************************/
         IF pin1 = "" then pin1 = "1234".
         if pin2 = "" then pin2 = "4321".

         CREATE IMSI.
         ASSIGN
            IMSI.IMSI       = IMSI
            IMSI.ICC        = icc
            IMSI.CustNum    = 0      /* NOT sold yet */
            IMSI.PIN1       = pin1
            IMSI.PIN2       = pin2
            IMSI.PUK1       = puk1
            IMSI.PUK2       = puk2
            IMSI.KI         = KI.
         PAUSE 0.

         /****************************
         * Add the Stock Balance     *
         ****************************/

         FIND StoBal WHERE 
            Stobal.Brand  = gcBrand AND 
            StoBal.SimArt = IFiSpx.SimArt  AND
            StoBal.StoBal = Stock
            Exclusive-lock No-error.
         IF NOT AVAIL StoBal THEN 
         DO:
            CREATE StoBal.
            ASSIGN
               Stobal.Brand  = gcBrand 
               StoBal.SimArt = IFiSpx.SimArt
               StoBal.StoBal = Stock.
         END.
         ASSIGN 
            StoBal.Balance    = StoBal.Balance + 1
            StoBal.DetBal[1] =  StoBal.DetBal[1] + 1.

         /***********************************
         * Add the total Balance of this    *
         * SIM article                      *
         ***********************************/

         FIND SimArt WHERE 
            SimArt.Brand  = gcBrand AND 
            SimArt.SimArt = IFiSpx.SimArt
            EXCLUSIVE-LOCK.
         ASSIGN SimArt.Balance   = SimArt.Balance +  1
                SimArt.DetBal[1] = SimArt.DetBal[1] + 1.
         RELEASE SimArt.
      END.
      END. /* TRANSACTION */
      INPUT STREAM SIM CLOSE.

      IF lError THEN
      DO:
         MESSAGE lcErrorMsg VIEW-AS ALERT-BOX BUTTONS OK.
         IF toimi = 5 THEN
            LEAVE PhaseLoop.
      END.

      IF phase =  2 THEN DO TRANS:

         CREATE ActionLog.
         ASSIGN 
            ActionLog.Brand        = gcBrand   
            ActionLog.TableName    = "SIMBatch"  
            ActionLog.KeyValue     = STRING(isimBatch)
            ActionLog.UserCode     = katun
            ActionLog.ActionID     = "SIMFILE"
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
            ActionLog.ActionChar   = "Totally " + STRING(tot) +
               " SIM records were successfully loaded" + CHR(10) +
               "Stock: " + Stock + CHR(10) +
               "Status: " + STRING(iSimStat) + " " + SIMStat.SSName + CHR(10) +
               "File: " + SIMFile
            ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED} 
            ActionLog.ActionTS     = fMakeTS().
         RELEASE ActionLog.   

         MESSAGE "Totally" tot "SIM records were successfully loaded" 
            VIEW-AS alert-box information title " LOAD COMPLETED ".
      END.
   END. /* phase ... */

   IF toimi = 4 THEN 
   DO:
      
      IF lError THEN 
         MESSAGE lcErrorMsg VIEW-AS ALERT-BOX BUTTONS OK.

      IF d-icc + d-IMSI > 0 THEN 
      DO:
         /* Duplicate ICCs ? */
         IF d-icc > 0 THEN 
            MESSAGE "Totally" tot "SIM records were read." SKIP
               d-icc "ICC codes were found already in"  SKIP
               "the SIM database.  File cannot be loaded."
               VIEW-AS ALERT-BOX error.

         /* Duplicate IMSIs ? */
         IF d-IMSI > 0 THEN 
            MESSAGE "Totally" tot "SIM records were read." SKIP
               d-IMSI "IMSI codes were found already in"  SKIP
               "the IMSI database.  File cannot be loaded."
               VIEW-AS ALERT-BOX error.
      END.
      ELSE  
      DO:     
         IF NOT lError THEN
            MESSAGE 
            "Totally" tot "SIM records were succesfully read" 
            VIEW-AS alert-box information title " File OK, NO ERRORS ".
      END.

      phase = -1. /* to prevent going back to update mode */
   END.

   IF toimi = 5 AND d-icc + d-IMSI = 0 AND NOT lError THEN 
      lRealUpdate = TRUE.

   IF toimi = 5 OR d-icc + d-IMSI > 0 THEN LEAVE Main.
END. /* Main */

HIDE MESSAGE.

HIDE FRAME main no-pause.

IF lRealUpdate THEN
DO:
   UNIX SILENT VALUE("mv " + SIMFile + " " + ProcessedSIMFile).
   RETURN "REAL".
END.
ELSE
   RETURN "".

/** This procedure ensures with version number that
*   processed filename does not exist in the SimBatch table
*   or in the directory where the file is moved after processing 
*   
*   @param pcSIMFileNamePart    the name part of the SIM file path
*          that may change when adding version number to the file name
*   @param pcProcessedSIMFileDir  the directory where the processed SIM
*          file is moved
*   @param pcSIMFileNamePartNoExt   the name of the SIM file without extension
*   @param pcSIMFileExtPart         the SIM file extension
*/
PROCEDURE HandleUniqueSIMFileName:
   DEFINE INPUT-OUTPUT PARAMETER pcSIMFileNamePart AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER pcProcessedSIMFileDir AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER pcSIMFileNamePartNoExt AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER pcSIMFileExtPart AS CHARACTER NO-UNDO. 

   DEFINE VARIABLE lNeededVersion AS LOGICAL NO-UNDO. 
   lNeededVersion = FALSE.
   DEFINE VARIABLE iVersion AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cVersion AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cFullFilePath AS CHARACTER NO-UNDO. 
   iVersion = 1.


   cFullFilePath = pcProcessedSIMFileDir + pcSIMFileNamePart.

   FindUniqueFileName:
   REPEAT:
      IF NOT CAN-FIND(SimBatch WHERE 
                  SimBatch.Brand = gcBrand AND 
                  SimBatch.ManCode = IFiSpx.ManCode AND
                  SimBatch.FileName = pcSIMFileNamePart) 
         AND SEARCH(cFullFilePath) = ? 
      THEN
         LEAVE FindUniqueFileName.

      FindNotExistFile:
      REPEAT:
         lNeededVersion = TRUE.
         IF iVersion < 10 THEN
            cVersion = "0" + STRING(iVersion).
         ELSE
            cVersion = STRING(iVersion).
         pcSIMFileNamePart = pcSIMFileNamePartNoExt + "." + 
                            pcSIMFileExtPart + cVersion.
         cFullFilePath = pcProcessedSIMFileDir + pcSIMFileNamePart.

         IF SEARCH(cFullFilePath) = ? THEN
         DO:
            pcSIMFileNamePartNoExt = pcSIMFileNamePartNoExt + cVersion.
            LEAVE FindNotExistFile.
         END.

         iVersion = iVersion + 1.         
      END.
   END.

   /* If filename needed change, the new filename is updated to the output
      parameter */
   IF lNeededVersion THEN
   DO:
      ProcessedSIMFile = cFullFilePath.
   END.
END.

/** This procedure splits the path of a file to its directory, name part,
* name part without extension and extension.
* 
* @param pcPath   the path of a file
* @param pcDir    the directory of the file
* @param pcFileName  the filename of the file ( no directory )
* @param pcNamePartNoExt  the filename of the file ( no directory or extension )
* @param pcExtPart   the extension of the filename
*/
PROCEDURE SplitFilePath:
   DEFINE INPUT PARAMETER pcPath AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER pcDir AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER pcFileName AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER pcNamePartNoExt AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER pcExtPart AS CHARACTER NO-UNDO. 

   DEFINE VARIABLE iNumDirEntries AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iNumCommaEntries AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iEntry AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cEntry AS CHARACTER NO-UNDO. 
   
   iNumDirEntries = NUM-ENTRIES(pcPath, "/").
   REPEAT iEntry = 1 TO iNumDirEntries - 1:
      cEntry = ENTRY(iEntry, pcPath, "/").
      IF iEntry = 1 THEN
         pcDir = cEntry.
      ELSE
         pcDir = pcDir + "/" + cEntry.
   END.
   pcFileName = ENTRY(iNumDirEntries, pcPath, "/").

   iNumCommaEntries = NUM-ENTRIES(pcFileName, ".").
   REPEAT iEntry = 1 TO iNumCommaEntries - 1:
      cEntry = ENTRY(iEntry, pcFileName, ".").
      IF iEntry = 1 THEN
         pcNamePartNoExt = cEntry.
      ELSE
         pcNamePartNoExt = pcNamePartNoExt + "." + cEntry.
   END. /* REPEAT iEntry */
   pcExtPart = ENTRY(iNumCommaEntries, pcFileName, ".").
END.



