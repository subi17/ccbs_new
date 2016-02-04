/*------------------------------------------------------------
  MODULE .......: clilog.i
  FUNCTION .....: Writes the logmessage TO a logfile OR
                  TO an EMail depending on the flag ilLogType
                  which is set BY TMSParam (in cliinitlog.i).
   USAGE:         RUN Mf/clilog.p 
                    (INPUT "FileName",     /* IF FileName should be shown */
                                           /* enter FileName here, mabye  */
                                           /* WITH FUNCTION PROGRAM-NAME(1) */
                     INPUT "The logmessage", 
                     INPUT 001,            /* IF applicable, ErrCode  */
                     INPUT YES,            /* Yes OR No depending on IF */
                                           /* Date will be shown        */
                                           /* in the LOG.               */
                     INPUT YES).           /* Yes/No depending on IF an */
                                           /* EMail will be Sent OR NOT */
  APPLICATION ..: 
  AUTHOR .......: DW Orbil
  CREATED ......: 22.09.1999 
  MODIFIED .....: 26.10.1999 1.0b   * Changed message-format TO X(200)
  Version ......: M15
--------------------------------------------------------------*/

{Syst/commali.i}
{Mf/cliinlog.i}

DEFINE INPUT PARAMETER pFileName  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pMessage   AS CHARACTER FORMAT "X(200)" NO-UNDO.
DEFINE INPUT PARAMETER pErrorCode AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER CommPaymDate      AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER pEmail     AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lAddress AS CHARACTER NO-UNDO.

IF pErrorCode <> 0 THEN DO:

   FIND FIRST CLIErrorCode WHERE 
              CLIErrorCode.ErrCode = pErrorCode 
   NO-LOCK NO-ERROR.
   IF AVAILABLE CLIErrorCode THEN
      pMessage = pMessage + " " + STRING(pErrorCode,"999") + " " + 
                CLIErrorCode.ErrMsg.
   ELSE 
      pMessage = pMessage + " " + STRING(pErrorCode,"999") + 
                 ", unknown error".
END.

IF pFileName <> "" THEN pMessage = pFileName + ". " + pMessage.

/* Always add time TO the MESSAGE */
pMessage = " " + STRING(TIME,"HH:MM:SS") + "  " + pMessage.

IF CommPaymDate THEN pMessage = STRING(TODAY,"99/99/99") + pMessage.
ELSE pMessage = "        " + pMessage.

IF ilLogType BEGINS "File" THEN
   PUT STREAM ilsLog pMessage SKIP.

IF ilLogType = "FileEmail" AND pEmail THEN DO:

   OUTPUT STREAM ilsLog CLOSE.

   /* WORKAROUND...! Make an explicit PAUSE,  */
   /* which will give the STREAM TO CLOSE     */
   /* DOWN properly BEFORE mailing the File...*/

   PAUSE 2 NO-MESSAGE.

   /* Look FOR the emailaddress in TMSParam */

   {Func/tmsparam.i NotifyAddress RETURN}

   lAddress = TMSParam.CharVal.

   RUN Mf/climail.p (INPUT lAddress,
                  INPUT "Cli updatestatus",
                  INPUT ilDestination).

END.

