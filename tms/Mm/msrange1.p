/* ----------------------------------------------------------------------
  MODULE .......: MSRange1.P
  TASK .........: Search FOR free MSISDN nos FOR a new MSRange for CustNum
                  and POS chosen. 
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 23-06-99
  CHANGED ......: 30-06-99
                  11-09-03 jp Brand
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/msisdn.i}
{Func/cparam2.i}

DEF INPUT  PARAMETER ms-code-res AS INTEGER                       NO-UNDO.
DEF INPUT  PARAMETER CustNum     AS INTEGER                       NO-UNDO.
DEF OUTPUT PARAMETER rc          AS lo                            NO-UNDO.
DEF OUTPUT PARAMETER MSISDN1     AS CHARACTER                     NO-UNDO.
DEF OUTPUT PARAMETER MSISDN2     AS CHARACTER                     NO-UNDO.
DEF OUTPUT PARAMETER exdate      AS DATE      FORMAT "99-99-99" NO-UNDO.
DEF OUTPUT PARAMETER ocPosCode   AS CHARACTER FORMAT "X(15)"      NO-UNDO.

DEF BUFFER x-MSISDN FOR MSISDN.

DEF VAR i               AS INTEGER                   NO-UNDO.
DEF VAR ml              AS INTEGER                   NO-UNDO.
def var Size            AS INTEGER   format "zzzz9"  NO-UNDO.
DEF VAR mstart          LIKE MSISDN.CLI              NO-UNDO.
DEF VAR lowlimit        LIKE MSISDN.CLI              NO-UNDO.
DEF VAR hilimit         LIKE MSISDN.CLI              NO-UNDO.
def var mask            AS CHARACTER format "x(5)"   NO-UNDO.
def var ms-code1        AS INTEGER   format "zz9"    NO-UNDO.
def var ms-code2        AS INTEGER   format "zz9"    NO-UNDO.
def var mc-code1        AS INTEGER   format "zz9"    NO-UNDO.
def var mc-code2        AS INTEGER   format "zz9"    NO-UNDO.

DEF VAR list            AS Longchar                  NO-UNDO.
DEF VAr liloop          AS INT                       NO-UNDO.
DEF VAR lcCode          AS CHARACTER                 NO-UNDO.
DEF VAR lcMSCode        AS CHARACTER                 NO-UNDO. 
DEF VAR lcMSCodeDesc    AS CHARACTER                 NO-UNDO. 
DEF VAR lcResPosDesc    AS CHARACTER                 NO-UNDO. 
DEF VAR cAllPosValues   AS CHARACTER                 NO-UNDO. 
DEF VAR iRankValueCount AS INTEGER                   NO-UNDO. 

DEF VAR cSelectionRankValueList     AS CHARACTER     NO-UNDO. 
DEF VAR cAllRankValueList           AS CHARACTER     NO-UNDO.
DEF VAR cReservingRankValueList     AS CHARACTER     NO-UNDO. 

cAllRankValueList = "0,1".

/* Default values for user input */
lowlimit   = fcParam("MSISDN", "AddLowLimit").
hilimit   = fcParam("MSISDN", "AddHiLimit").

/* Value used for new MSRange records to be created */
DEFINE VARIABLE cExpDate AS CHARACTER /* FORMAT "99-99-99"*/ NO-UNDO. 
cExpDate = fcParam("MSISDN", "ExpireDate").
exDate     = Date(cExpDate).
/* EMPTY TEMP-TABLE ttChooseRange. */

/* Gather all POS values from TMSCodes table to cAllPosValues
   comma separated list */
cAllPosValues = "".
FOR EACH TMSCodes WHERE 
   TMSCodes.TableName = "MSISDN" AND 
   TMSCodes.FieldName = "POS" NO-LOCK:
   IF cAllPosValues = "" THEN
      cAllPosValues = TMSCodes.CodeValue.
   ELSE
      cAllPosValues = cAllPosValues + "," + TMSCodes.CodeValue.
END. /* FOR EACH TMSCodes */

/* Count Rank values from TMSCodes table for user input
   validation; it is assumed that ranks are integers from
   0 to n with increment by 1 between all rank values. */
iRankValueCount = 0.
FOR EACH TMSCodes WHERE 
   TableName = "MSISDNNumber" AND FieldName = "Rank" NO-LOCK:
   IF cSelectionRankValueList = "" THEN
      cSelectionRankValueList  = TmsCodes.CodeValue.
   ELSE
      cSelectionRankValueList  = cSelectionRankValueList + 
         "," + TmsCodes.CodeValue.
     
   iRankValueCount = iRankValueCount + 1.
END.


/**
* This function returns TRUE, if the input string contains only
* digit characters.
*
* @param cInput  Input string
* @return TRUE, if the input contains only digit characters.
*/
FUNCTION chkOnlyDigits RETURNS LOGICAL (INPUT cInput AS CHARACTER):
   DEFINE VARIABLE liLength AS INTEGER NO-UNDO. 
   DEFINE VARIABLE li AS INTEGER NO-UNDO. 
   DEFINE VARIABLE chkC AS CHARACTER NO-UNDO. 

   liLength = LENGTH(cInput).
   REPEAT li = 1 TO liLength:
      chkC = SUBSTRING(cInput, li, 1).
      IF chkC < "0" OR chkC > "9" THEN RETURN FALSE.
   END. /* REPEAT li */
   
   RETURN TRUE.
END. /* PROCEDURE chkOnlyDigits */


rc = FALSE.

/* set the status code FOR a unreserved MSISDN No. */
{Func/tmsparam.i MSStatusUnr return}.  
ms-code1 = tmsparam.intVal. 
ms-code2 = ms-code1.
mc-code1 = 0. 

FIND MSStat WHERE MSStat.StatusCode = ms-code1 no-lock no-error.
IF NOT AVAIL MSStat THEN 
DO:
   MESSAGE "MSISDN Status Code"  ms-code1 "is defined as a " SKIP
      "default code for an UNRESERVED MSISDN No.      " SKIP
      "in System Parameter PaymFile But it is not found"  SKIP
      "in the MSISDN Status Code PaymFile"
      view-as alert-box error title " INVALID PARAMETER VALUE ".
   RETURN.
END. /* IF NOT AVAIL MSStat */

DEFINE FRAME range WITH WIDTH 80.

FORM
"Note:  This program searches the MSISDN number plan "            SKIP
"       in order to find a not yet reserved range    "            SKIP
"       of numbers."                                              SKIP(1)
"   Amount of needed MSISDN .....:"  Size        
HELP "How many MSISDN numbers are needed (i.e. Size of a Range)"  SKIP
"   MSISDN shall be beyond ......:"  lowlimit     
HELP "Possible lower Limit of a single MSISDN"                    SKIP
"   MSISDN shall be below .......:"  hilimit     
HELP "Possible upper Limit of a single MSISDN"                    SKIP
"   MSISDN Classes (Rank)........:"  lcMSCode FORMAT "X(3)"
HELP "Number rank (F9)"              lcMSCodeDesc FORMAT "X(30)"  SKIP
"   Reservation POS Code.........:"  ocPosCode FORMAT "X(12)"        
HELP "Reservation position (F9)"     lcResPosDesc FORMAT "X(29)"  SKIP
   WITH
      OVERLAY 
      CENTERED 
      ROW 5 
      NO-LABELS 
      TITLE " FIND A NEW MSISDN RANGE "
      FRAME range.

PAUSE 0.

IF lowLimit = ? OR lowLimit = "" THEN lowLimit = "000000000".
IF hiLimit = ?  OR hiLimit = ""  THEN hiLimit  = "999999999".

ASSIGN 
   SIZE = 1
   /* mask = "00" */ 
   ms-code1 = 0
   ms-code2 = 1.

DEFINE VARIABLE iLcCode AS INTEGER NO-UNDO. 


/* This function is used to check whether user input for Rank
*   is correct. Values "ALL" or integer values from 0 to maximum Rank Value
*   read from TMSCodes (iRankValue) are defined to be correct.
*   Empty value is not correct. 
*   @param lcRank  user input for Rank
*   @return TRUE, if the user input is correct
*/
FUNCTION CheckRankInput RETURNS LOGICAL (INPUT lcRank AS CHARACTER):
   DEFINE VARIABLE cTrimmedValue AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE iRank AS INTEGER NO-UNDO. 

   cTrimmedValue = TRIM(lcRank).
   IF cTrimmedValue = "ALL" THEN RETURN TRUE.
   IF LENGTH(cTrimmedValue) = 0 THEN RETURN FALSE.
   
   iRank = INTEGER(cTrimmedValue) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN FALSE.
   IF iRank >= 0 AND iLcCode < iRankValueCount THEN RETURN TRUE.
   
   RETURN FALSE.
END.


MAIN:
REPEAT WITH FRAME range.
   ehto = 9.  RUN Syst/ufkey.

   UPDATE 
      Size 
      lowlimit VALIDATE(INPUT hilimit >= INPUT lowlimit AND
         chkOnlyDigits(INPUT lowlimit), 
         "MSISDN contains only digits, " + 
         "low limit must be less than or equal to high limit")
      hilimit  VALIDATE(INPUT hilimit >= INPUT lowlimit AND
         chkOnlyDigits(INPUT hilimit),
         "MSISDN contains only digits, " +
         "high limit must be greater than or equal to low limit")
      lcMsCode VALIDATE( CheckRankInput( INPUT lcMsCode ), 
         "Choose entry with F9")
      ocPosCode VALIDATE(LOOKUP(TRIM(INPUT ocPosCode), cAllPosValues) > 0,
         "Choose entry with F9") 
   WITH FRAME range
   EDITING:
      READKEY.
      
      DEFINE VARIABLE cLastKeyLabel AS CHARACTER NO-UNDO. 
      cLastKeyLabel = KEYLABEL(LASTKEY).

      /* Rank input checking when typing; description generated */
      IF FRAME-FIELD = "lcMSCode" AND LOOKUP(cLastKeyLabel, poisnap) > 0 THEN
      DO:
          DEFINE VARIABLE inputMsCode AS CHARACTER NO-UNDO. 
          
          inputMsCode = TRIM(INPUT lcMsCode).
          lcMsCode = inputMsCode.

          DISPLAY lcMsCode.

          lcMSCodeDesc = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                            "MSISDNNumber","Rank", inputMsCode).
          DISPLAY lcMSCodeDesc.
      END. /* IF FRAME-FIELD = "lcMSCode" AND LOOKUP(... */

      /* Rank input checking when using F9; description generated */
      IF FRAME-FIELD = "lcMSCode" AND cLastKeyLabel = "F9" THEN 
      DO:
         RUN Help/h-tmscodes(INPUT "MSISDNNumber",  /* TableName */
                              "Rank",  /* FieldName */
                              "Rank",   /* GroupCode */
                       OUTPUT lcCode).
         
         IF lcCode NE "" AND lcCode NE ? THEN
         DO:
            lcMsCode = lcCode.
            DISPLAY lcMsCode WITH FRAME range.
         END. /* IF lcCode NE "" ... */
         
         lcMSCodeDesc = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                           "MSISDNNumber","Rank", lcCode).
         
         DISPLAY lcMSCodeDesc. 
      END. /* IF FRAME-FIELD = "lcMSCode" AND cLastKeyLabel */

      /* POS code input checking when typing; description generated */
      IF FRAME-FIELD = "ocPosCode" AND LOOKUP(cLastKeyLabel, poisnap) > 0 THEN
      DO:
          DEFINE VARIABLE inputOsCode AS CHARACTER NO-UNDO. 
          
          inputOsCode = TRIM(INPUT ocPosCode).
          ocPosCode = inputOsCode. 

          DEFINE VARIABLE inputMsCode2 AS CHARACTER NO-UNDO. 
          
          inputMsCode2 = TRIM(INPUT lcMsCode).
          lcMsCode = inputMsCode2.

          lcResPosDesc = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                            "MSISDN","POS", inputOsCode).

          DISPLAY lcResPosDesc.
          DISPLAY ocPosCode.
      END. /* IF FRAME-FIELD = "ocPosCode" AND LOOKUP( */

      /* POS code input checking when using F9; description generated */
      IF FRAME-FIELD = "ocPosCode" AND cLastKeyLabel = "F9" THEN 
      DO:
         RUN Help/h-tmscodes(INPUT "MSISDN",  /* TableName */
                              "POS",  /* FieldName */
                              "Reservation Position",   /* GroupCode */
                       OUTPUT lcCode).
         
         IF lcCode NE "" AND lcCode NE ? THEN
         DO:
            ocPosCode = lcCode.
            DISPLAY ocPosCode WITH FRAME range.
         END. /* IF lcCode NE "" */
         
         lcResPosDesc = DYNAMIC-FUNCTION("fTMSCodeName" in ghFunc1,
                           "MSISDN", "POS", lcCode).
         
         DISPLAY lcResPosDesc. 
      END. /* IF FRAME-FIELD = "ocPosCode" AND cLastKeyLabel */

      APPLY LASTKEY.
      ehto = 9.
      RUN Syst/ufkey.
   END. /* EDITING */

ACTION:
   REPEAT WITH FRAME range.
      ASSIGN ehto = 0 ufk = 0 ufk[1] = 7 ufk[5] =  15 ufk[8] = 8.
      RUN Syst/ufkey.
      IF toimi = 1 THEN NEXT main.
      IF toimi = 8 THEN LEAVE main.
      IF TOIMI = 5 THEN DO:
         IF Size =  0 THEN 
         DO:
            MESSAGE
               "You have not determined the Size of range !"
               VIEW-AS ALERT-BOX ERROR.
            NEXT Action.
         END. /* IF Size = 0 */  

         LEAVE Action.
      END. /* IF toimi = 5 */
   END. /* REPEAT WITH FRAME range, Action */

   DEF VAR minMSISDN AS DEC NO-UNDO.
   DEF VAR maxMSISDN AS DEC NO-UNDO.

   ocPosCode = CAPS(TRIM(ocPosCode)).
   lcMSCode = CAPS(TRIM(lcMSCode)).

   /* Seek possible MSRange:s to the temptable ttChooseRange 
      by seeking the ranges in test mode through the whole
      CLI range given in the user interface */

   IF lcMSCode = "9" THEN
     cReservingRankValueList = "0,1".
   ELSE 
     cReservingRankValueList = lcMSCode.

         RUN MarkRankedMSISDNInRange(
             FALSE,
             lowLimit, hiLimit, 
             ms-code1, ms-code2,
             cReservingRankValueList, 
             size,
             CustNum, ocPosCode,
             OUTPUT minMSISDN,
             OUTPUT maxMSISDN,
             OUTPUT liLoop).

         ASSIGN
            MSISDN1 = STRING(minMSISDN)
            MSISDN2 = STRING(maxMSISDN).
    
         IF liloop > 0 THEN
         DO:
            DEFINE VARIABLE lNoIndexClashEnsured AS LOGICAL NO-UNDO. 
            lNoIndexClashEnsured = FALSE.
            DO WHILE NOT lNoIndexClashEnsured:
               FIND FIRST MSRange WHERE CLIFrom = MSISDN1 NO-LOCK NO-ERROR.
               IF NOT AVAILABLE MSRange THEN
                 lNoIndexClashEnsured = TRUE.
               ELSE
               DO:
                  DEFINE VARIABLE iMSISDN1 AS INTEGER NO-UNDO.
                  iMSISDN1 = INTEGER(MSISDN1).
                  iMSISDN1 = iMSISDN1 - 1.
                  MSISDN1 = STRING(iMSISDN1).
               END.
            END.

            rc = TRUE.
            MESSAGE 
            "MSISDN number range "+  MSISDN1 + " - " + MSISDN2 + 
            " with " + STRING(liloop) + " MSISDN numbers" SKIP
            "is reserved for " + lcResPosDesc + " (" + ocPosCode + ")" +
            "." VIEW-AS ALERT-BOX.
         END.
         ELSE
         DO:
            MESSAGE "No MSISDN record was found." VIEW-AS ALERT-BOX.
            rc = FALSE.
            LEAVE main.
         END. 

   rc = TRUE.
   LEAVE main.
END. /* REPEAT, MAIN */

HIDE FRAME range NO-PAUSE.
HIDE MESSAGE.


/**
* This function performes the marking of the current MSISDN record.
* Marking is performed by moving the current MSISDN to the 
* the history (sets valid to current time) and creatin a new MSISDN
* with same CLI value and ValidFrom with current time (added by second).
* The new MSISDN is assigned given values for CustNum and POS and
* Status value 1.
*
* This function also updates the minimun and maximum limits of the 
* created MSRange record, the count of reserved MSISDN records.
* Last it is checked whether all the requested MSISDN records
* are reserved and TRUE is returned if that is the situation.
* 
* The MSISDN record buffer must point to correct record before 
* calling this function. 
*
* @param iCustNum   CustNum for the new MSISDN record.
* @param iPOSCode   POS for the new MSISDN record
* @param iMaxNumberOfRecords  The number of requested MSISDN records
* @param pNumberOfRecords   The number of reserved MSISDN records
* @param oMINMSISDN    The minimum (CLIFrom) of the created MSRange
*                      record
* @param oMaxMSISDN    The maximum (CLITo) of the created MSRange
*                      record
* @return TRUE if all the requested MSISDN records are reserved.
*/
FUNCTION MarkMSISDN RETURNS LOGICAL  
   (INPUT iCustNum AS INTEGER,
    INPUT iPOSCode AS CHARACTER,
    INPUT iMaxNumberOfRecords AS INTEGER,
    INPUT-OUTPUT pNumberOfRecords AS INTEGER,
    INPUT-OUTPUT oMINMSISDN AS DECIMAL,
    INPUT-OUTPUT oMaxMSISDN AS DECIMAL):

    fMakeMsidnHistory(RECID(X-MSISDN)).

    ASSIGN MSISDN.CustNum    = iCustNum
           MSISDN.StatusCode = 1
           MSISDN.POS        = iPosCode.

    ASSIGN
       oMinMSISDN = DEC( MIN( oMinMSISDN, DEC( X-MSISDN.CLI ) ) )
       oMaxMSISDN = DEC( MAX( oMaxMSISDN, DEC( X-MSISDN.CLI ) ) ).

    pNumberOfRecords = pNumberOfRecords + 1.

    put screen row 1 string(pNumberOfRecords).
    IF pNumberOfRecords >= iMaxNumberOfRecords THEN 
       RETURN TRUE.
    RETURN FALSE.
END.

/**
*  This procedure marks or tests how to mark/reserve (lTestRun=TRUE) 
*  MSISDN records between the given CLI values, Status and 
*  corresponding MSISDNNumber.Rank values for the given POS and CustNum. 
*
*  The marking is performed from the lowest MSISDN CLI value towards 
*  the highest MSISDN CLI value given until some condition stops 
*  the reservation. MarkMSISDN function is used for the 
*  real marking/reservation.
*  
*  The procedure takes the number of requested MSISDN records as an 
*  input parameter and returns the number of reserved MSISDN records
*  and the CLI value limits for the MSRange record to be created.
*
*  Conditions for checking the reservation/marking:
*
*  - Only the newest MSISDN with the same CLI value among some other 
*    MSISDN records is checked for reservation. 
*  - The reservation is not checked for MSISDN records with Status 
*     value outside the given Status value range 
*  - The reservation is not checked for POS values that are not empty.
*
*  If condition is not checked for reservation, it does not stop 
*  the reservation.
*
*  Conditions for stopping the reservation/marking:
*
*  If this procedure finds after some reservations that it encounters
*
*  - MSISDN record with different or missing Rank or 
*  - MSISDN record that is already reserved to an existing MSRange record
*  - the situation where the number of requested MSISDN records is reserved.
*
*  it stops the reservation to prevent MSRange records intercepting 
*  each other with their CLI value limits or because the task gets
*  fully succeeded.
*
*  @param lTestRun  TRUE if the MSISDN records are not truely reserved
*                   but only tested which kind of MSRange could be 
*                   created. FALSE for true reservation.
*  @param icBeginCLI   The low limit for the CLI values of the reserved 
*                      MSISDN records
*  @param icEndCLI     The high limit for the CLI values of the reserved 
*                      MSISDN records
*  @param iBeginStatus  The low limit of the Status values of the reserved
*                       MSISDN records.
*  @param iEndStatus    The hi limit of the Status values of the reserved
*                       MSISDN records.
*  @param iMcCode       The Rank value for which the MSISDN records 
*                       are reserved. The Rank value for the MSISDN record
*                       is found from the MSISDNNumber record having 
*                       the same CLI value as the MSISDN record.
*  @param iNumberOfRecords  The number of requested MSISDN records.
*  @param iCustNum          CustNum for which MSISDN records are reserved.
*  @param iPosCode          POS for which MSISDN records are reserved.
*  @param oMinMSISDN        The minimum CLI value (CLIFrom) for the 
*                           MSRange record to be created.
*  @param oMaxMSISDN        The maximum CLI value (CLITo) for the 
*                           MSRange record to be created.
*  @param oNumberOfRecords  The number of reserved MSISDN records.
*/
PROCEDURE MarkRankedMSISDNInRange:
   DEFINE INPUT PARAMETER lTestRun   AS LOGICAL NO-UNDO. 
   DEFINE INPUT PARAMETER icBeginCLI AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icEndCLI AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iBeginStatus AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER iEndStatus AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER iMcCodeList AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iNumberOfRecords AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER iCustNum AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER iPosCode AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER oMinMSISDN AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER oMaxMSISDN AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER oNumberOfRecords AS INTEGER NO-UNDO.
   
   ASSIGN 
      oMinMSISDN = 999999999
      oMaxmsisdn = 0
      oNumberOfRecords = 0.

   LoopReserveRankedMSISDN:
   FOR EACH x-MSISDN 
      NO-LOCK 
      USE-INDEX CLI 
      WHERE
         x-MSISDN.CLI         >= icBeginCLI    AND
         x-MSISDN.CLI         <=  icEndCLI     AND
         x-MSISDN.StatusCode  >= iBeginStatus  AND
         x-MSISDN.StatusCode  <= iEndStatus    AND
         x-MSISDN.Brand        = gcBrand       AND 
         x-MSISDN.ValidTo      > fMakeTS()     AND 
         x-MSISDN.pos          = ""
      BREAK BY x-MSISDN.CLI:
         
      IF FIRST-OF(x-MSISDN.CLI) THEN
      DO:

         FIND FIRST MSISDNNumber OF x-MSISDN NO-LOCK 
            WHERE LOOKUP(STRING(MSISDNNumber.Rank), iMcCodeList) > 0 NO-ERROR.

         IF AVAILABLE MSISDNNumber THEN
         DO:
            IF NOT lTestRun THEN 
            DO:
              IF MarkMSISDN( CustNum, iPosCode, iNumberOfRecords,
                              INPUT-OUTPUT oNumberOfRecords,
                              INPUT-OUTPUT oMinMSISDN,
                              INPUT-OUTPUT oMaxMSISDN ) THEN                                    
                 LEAVE LoopReserveRankedMSISDN.
            END.
            ELSE
            DO:
              ASSIGN
                  oNumberOfRecords = oNumberOfRecords + 1
                  oMinMSISDN = DEC( MIN( oMinMSISDN, DEC( X-MSISDN.CLI ) ) )
                  oMaxMSISDN = DEC( MAX( oMaxMSISDN, DEC( X-MSISDN.CLI ) ) ).
              IF oNumberOfRecords >= iNumberOfRecords THEN
                 LEAVE LoopReserveRankedMSISDN.
            END.
         END.
       END.
   END. /* FOR EACH x-MSISDN */
END.
