/*------------------------------------------------------------
  MODULE .......: clicrrej.p 
  FUNCTION .....: Creates a Rejectfile. (When the File is NOT
                  valid).
  APPLICATION ..: 
  AUTHOR .......: PG, DW Orbil
  CREATED ......: 20.09.1999 kl
  MODIFIED .....: 
  Version ......: M15
--------------------------------------------------------------*/
{Syst/commali.i}

DEFINE INPUT    PARAMETER pFileName     AS CHARACTER    NO-UNDO.
DEFINE INPUT    PARAMETER pErrorCode    AS INTEGER      NO-UNDO.

DEFINE VARIABLE lResult                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMessage                AS CHARACTER NO-UNDO.

DEFINE STREAM   sIn.
DEFINE STREAM   sOut.
DEFINE VARIABLE lLength                 AS INTEGER      NO-UNDO.
DEFINE VARIABLE lRecord                 AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lFile                   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lResponseDir            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lCliFileDir             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lCliFileSeparator       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lCreateDate_Time        AS CHARACTER    NO-UNDO.

{Func/tmsparam.i CliResponseDir RETURN}.
lResponseDir = TMSParam.CharVal.

{Func/tmsparam.i CliFileDir RETURN}
lCliFileDir = TMSParam.CharVal.

/* Construct a reject File Name. Same as parameter pFileName except "ERR" before the dot */ 
lFile = pFileName.
lLength = LENGTH(lFile).
SUBSTRING(lFile , lLength - 6, 3 ) = "ERR".

lFile = lResponseDir + "/" + lFile. /* Add full path TO the out File Name */

OUTPUT STREAM   sOut    TO VALUE(lFile).
INPUT STREAM    sIn     FROM VALUE(lCliFileDir + "/" + pFileName).

{Func/tmsparam.i CliFileSeparator RETURN}.
lCliFileSeparator = TMSParam.CharVal.

lResult = "Error".
FIND FIRST CLIErrorCode
     WHERE CLIErrorCode.ErrCode = pErrorCode NO-LOCK NO-ERROR.
IF AVAILABLE CLIErrorCode THEN DO:
    lMessage = CLIErrorCode.ErrMsg.
END.
ELSE
    lMessage = "Errorcodemessage not found.".

/* Save the creationdate AND time of the File */    
lCreateDate_time = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(TIME,"HH:MM:SS").
lCreateDate_time = REPLACE(lCreateDate_time , ":" , "" ).

REPEAT:

    IMPORT STREAM sIn UNFORMATTED lRecord.

    CASE ENTRY(1,lRecord,lCliFileSeparator):
        WHEN "HDR" THEN DO:

            /*PUT STREAM sOut UNFORMATTED lRecord SKIP. */

            PUT STREAM sOut UNFORMATTED
                ENTRY(1,lRecord,lcliFileSeparator)   lcliFileSeparator
                ENTRY(2,lRecord,lcliFileSeparator)   lcliFileSeparator
                ENTRY(3,lRecord,lcliFileSeparator)   lcliFileSeparator
                ENTRY(4,lRecord,lcliFileSeparator)   lcliFileSeparator
                lCreateDate_Time    SKIP.        

        END. /* END WHEN "HDR" */

        WHEN "DTL" THEN DO:
            PUT STREAM sOut UNFORMATTED
                ENTRY(4,lRecord,lCliFileSeparator)  lCliFileSeparator
                lResult                             lCliFileSeparator
                pErrorCode                          lCliFileSeparator
                lMessage                            lCliFileSeparator
                ENTRY(3,lRecord,lCliFileSeparator)  SKIP.                
        END. /* END WHEN "DTL" */

        WHEN "TLR" THEN DO:
        /*    PUT STREAM sOut UNFORMATTED lRecord SKIP.*/

            PUT STREAM sOut UNFORMATTED
                ENTRY(1,lRecord,lcliFileSeparator)   lcliFileSeparator
                ENTRY(2,lRecord,lcliFileSeparator)   lcliFileSeparator
                ENTRY(3,lRecord,lcliFileSeparator)   lcliFileSeparator
                ENTRY(4,lRecord,lcliFileSeparator)   lcliFileSeparator
                lCreateDate_Time    SKIP.        

        END. /* END WHEN "TLR" */

        OTHERWISE DO:
            PUT STREAM sOut UNFORMATTED
                "Unknown record RepCliSimType: " + ENTRY(1,lRecord,lCliFileSeparator) SKIP.
        END. /* END OTHERWISE */

    END.    /* END CASE */
END.  /* END REPEAT  */
INPUT   STREAM sIn  CLOSE.
OUTPUT  STREAM sOut CLOSE.

/* Get the Name of the ftp-script from TMSParam. */
{Func/tmsparam.i FtpPutResponseFile RETURN}

/* RUN the script that PUT the cli-file on the ftp-server. */
OS-COMMAND SILENT VALUE (TMSParam.CharVal + " " + lFile).    


