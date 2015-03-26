/*------------------------------------------------------------
  MODULE .......: clicrres.p
  FUNCTION .....: CREATE a responsefile.
  APPLICATION ..: 
  AUTHOR .......: PG, Orbil
  CREATED ......: 20.09.1999 kl
  MODIFIED .....: 02.11.1999 (1.0.d) Deleted the "Add Zero to CLI" funtionality
                  15.11.1999 kl detail ROW fixing
                  23.11.1999 kl upper()
                  23.08.2000 kl "OK"
                  28.08.2000 kl "OK" removed, was missing from clireadf

  Version ......: M15
--------------------------------------------------------------*/
{commali.i}
{function.i}

DEFINE INPUT    PARAMETER pOrderID  AS CHARACTER    NO-UNDO.
DEFINE INPUT    PARAMETER pCustNr   AS INTEGER      NO-UNDO.
DEFINE OUTPUT   PARAMETER lFileName AS CHARACTER    NO-UNDO.
DEFINE OUTPUT   PARAMETER pResult   AS LOGICAL      NO-UNDO. 

DEFINE STREAM   s1.
DEFINE VARIABLE CLIFileSeparator    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lResellerID         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lDetailCount        AS INTEGER      NO-UNDO.
DEFINE VARIABLE lCreateDate_Time    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lResponseDir        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lLength             AS INTEGER      NO-UNDO.
DEFINE VARIABLE lBuff               AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lFtpScriptName      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lInitSign           AS CHARACTER    NO-UNDO.

FIND FIRST rsoper WHERE
           rsoper.CustNum = pCustNr
NO-LOCK NO-ERROR.           
lResponseDir = fChkPath(rsoper.rs-dir) + "response/".

{tmsparam.i CLIFileSeparator RETURN}.
CLIFileSeparator = CharVal.

/* lCreateDate_time will be written TO the response PaymFile 
    TO indicate the PaymFile CREATE time. */
ASSIGN
   lCreateDate_time = STRING(YEAR(TODAY))       + 
                      STRING(MONTH(TODAY),"99") + 
                      STRING(DAY(TODAY),"99")   +
                      STRING(TIME,"HH:MM:SS")
   lCreateDate_time = REPLACE(lCreateDate_time , ":" , "" ).

FIND FIRST CLIFile WHERE 
           CLIFile.OrderId = pOrderID 
NO-LOCK NO-ERROR.
IF AVAILABLE CLIFile THEN DO:
   ASSIGN 
      lFileName    = CLIFile.FileName
      lDetailCount = CLIFile.DtlCount
      lResellerID  = CLIFile.RsId.
END. /* END IF */        
ELSE DO:
   pResult = FALSE.    
   RETURN.
END. /* END ELSE */    

/* Construct a response PaymFile Name. 
   Same as CLIFile.FileName except "RES" before the dot */ 
ASSIGN
   lLength = LENGTH(lFileName)
   SUBSTRING(lFileName , lLength - 6, 3 ) = "RES".

/* Add full path TO the out PaymFile Name */
lFileName = lResponseDir + lFileName. 

OUTPUT STREAM s1 TO VALUE(lFileName).   

FIND FIRST CLIQueue WHERE 
           CLIQueue.OrderId = pOrderID 
NO-LOCK NO-ERROR.

IF AVAILABLE CLIQueue THEN
   PUT STREAM s1 UNFORMATTED
      "HDR"               CLIFileSeparator
      upper(lResellerID)  CLIFileSeparator
      upper(pOrderID)     CLIFileSeparator
      "0"                 CLIFileSeparator
      lCreateDate_Time    SKIP.        
ELSE DO:
    pResult = FALSE.
    OUTPUT STREAM s1 CLOSE.
    RETURN.
END. /* END ELSE */    

FOR EACH CLIQueue NO-LOCK WHERE 
         CLIQueue.Order = pOrderID.

   PUT STREAM s1 UNFORMATTED
      "DTL"                       CLIFileSeparator
      CLIQueue.CLI                CLIFileSeparator
      upper(CLIQueue.Result)      CLIFileSeparator.

   /* ErrCode + messages ONLY WITH errors */
   IF CLIQueue.Result = "ERROR" THEN  
      PUT STREAM s1 UNFORMATTED
         CLIQueue.ErrCode           CLIFileSeparator
         upper(CLIQueue.ErrMsg) CLIFileSeparator.
   ELSE PUT STREAM s1 UNFORMATTED CLIFileSeparator CLIFileSeparator.

   PUT STREAM s1 UNFORMATTED upper(CLIQueue.Command) SKIP.

END. /* END FOR EACH */

PUT STREAM s1 UNFORMATTED
   "TLR"               CLIFileSeparator
   lResellerID         CLIFileSeparator
   pOrderID            CLIFileSeparator
   lDetailCount        CLIFileSeparator
   lCreateDate_Time    SKIP.

OUTPUT STREAM s1 CLOSE.

/* RUN the script that PUT the cli-file on the ftp-server. */
IF rsoper.res-script NE "" THEN
   OS-COMMAND SILENT VALUE(rsoper.res-script + " " + lFileName).

pResult = TRUE.




