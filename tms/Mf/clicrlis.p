/*------------------------------------------------------------
  MODULE .......: clicrlis.p 
  FUNCTION .....: CREATE a list that will be Sent TO ExCode TO
                  UPDATE white-list.
  APPLICATION ..: 
  AUTHOR .......: PG, DW Orbil
  CREATED ......: 20.09.1999
  MODIFIED .....: 02.11.1999    1.0g    * Deletelist-lowercase

  Version ......: M15
--------------------------------------------------------------*/
{commali.i}

DEFINE INPUT  PARAMETER pOrderID    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pResellerID AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pReturn     AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDirName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCli      AS CHARACTER NO-UNDO.

DEFINE VARIABLE sAXEAdd   AS CHARACTER NO-UNDO.
DEFINE VARIABLE sAXEDel   AS CHARACTER NO-UNDO.
DEFINE VARIABLE sDX2Add   AS CHARACTER NO-UNDO.
DEFINE VARIABLE sDX2Del   AS CHARACTER NO-UNDO.

DEFINE STREAM sFile.

{cliinlog.i}

FUNCTION fWL RETURNS CHARACTER
  (INPUT lFileName AS CHAR).

   DEF VAR Counter AS C  NO-UNDO INIT "01".

   CLI:
   REPEAT:
      IF NOT CAN-FIND(FIRST CLIWL WHERE  
                            CLIWL.FileName = lFileName + 
                                             Counter + ".TXT") THEN DO:
         lFileName = LC(lFileName + Counter + ".txt").
         CREATE CLIWL.
         ASSIGN 
            CLIWL.FileName  = lFileName
            CLIWL.OrderId   = pOrderID
            CLIWL.Processed = FALSE.               
         IF lFileName BEGINS "AXE" THEN
            CLIWL.ExCode = "AXE".
         ELSE
            CLIWL.ExCode = "DX200".
         LEAVE CLI.
      END. /* END IF */
      ELSE DO:
         Counter = STRING(INTEGER(Counter) + 1).
         IF INTEGER(Counter) < 10 THEN Counter = "0" + Counter.        
      END. /* END ELSE */

      IF INTEGER (Counter) > 99 THEN LEAVE CLI.

   END.   

   /* What's the meaning of this: CLIWL is just created ??? 
   IF CAN-FIND(FIRST CLIWL WHERE 
                     CLIWL.FileName BEGINS lFileName AND 
                     CLIWL.OrderId   = pOrderId      AND 
                     CLIWL.Processed = FALSE)        THEN DO:
      RUN clilog.p 
        (INPUT PROGRAM-NAME(1),
         INPUT "The list has already been created for OrderId " + 
               pOrderID + ", proceeds...",
         INPUT 0,       /* No ErrCode */
         INPUT NO,     /* NO = Date will NOT show in LOG */
         INPUT NO).    /* NO = EMail will NOT be Sent */

      pReturn = FALSE.

   END.
   */

   RETURN Counter.

END.

/* Get the Name of the directory where the Alist will be stored */
{tmsparam.i WLDir RETURN}.   
lDirName = TMSParam.CharVal .  /* Directory Name */ 

IF CAN-FIND(FIRST CLIQueue WHERE 
                  CLIQueue.OrderId   = pOrderId AND 
                  CLIQueue.Command   = "Add"    AND 
                  CLIQueue.ErrCode = 0        AND
                  CLIQueue.ExCode    = "AXE")   THEN
   ASSIGN
      lFileName = "AXEAdd" + STRING(TODAY,"999999")
      sAXEAdd   = fWL(lFileName).

IF CAN-FIND(FIRST CLIQueue WHERE 
                  CLIQueue.OrderId   = pOrderId AND 
                  CLIQueue.Command   = "Add"    AND 
                  CLIQueue.ErrCode = 0        AND
                  CLIQueue.ExCode    = "DX200") THEN
   ASSIGN
      lFileName = pResellerId + "Add" + STRING(TODAY,"999999")
      sDX2Add   = fWL(lFileName).

IF CAN-FIND(FIRST CLIQueue WHERE 
                  CLIQueue.OrderId   = pOrderId AND 
                  CLIQueue.Command   = "Delete" AND 
                  CLIQueue.ErrCode = 0        AND
                  CLIQueue.ExCode    = "AXE")   THEN 
   ASSIGN
      lFileName = "AXEDel" + STRING(TODAY,"999999")
      sAXEDel   = fWL(lFileName).

IF CAN-FIND(FIRST CLIQueue WHERE 
                  CLIQueue.OrderId   = pOrderId AND 
                  CLIQueue.Command   = "Delete" AND 
                  CLIQueue.ErrCode = 0        AND
                  CLIQueue.ExCode    = "DX200") THEN 
   ASSIGN
      lFileName = pResellerId + "Del" + STRING(TODAY,"999999")
      sDX2Del   = fWL(lFileName).

FOR EACH CLIQueue NO-LOCK WHERE 
         CLIQueue.OrderId   = pOrderID AND 
         CLIQueue.ErrCode = 0        AND
         CLIQueue.Command  NE "RACPS": /* NOT preselect commands */

   /* IF leading zero, remove it! */
   IF SUBSTRING(CLIQueue.CLI,1,1) = "0" THEN lCli = SUBSTRING(CLIQueue.CLI,2).

   IF CLIQueue.Command = "Add" THEN DO:
      /* NOKIA ExCode */
      IF clique.ExCode = "DX200" THEN DO:
         ASSIGN lFileName = pResellerId + "Add" + STRING(TODAY,"999999") +
                            sDX2Add + ".TXT".
         OUTPUT STREAM sFile TO VALUE(lDirName + "/" + LC(lFileName)) APPEND.
         PUT STREAM sFile UNFORMATTED lCli. 
         PUT STREAM sFile CONTROL "~015" "~012".
         OUTPUT STREAM sFile CLOSE.
      END.
      /* ERICSSON ExCode */
      ELSE IF clique.ExCode = "AXE"   THEN DO:
         ASSIGN lFileName = "AXEAdd" + STRING(TODAY,"999999") +
                            sAXEAdd + ".TXT".
         OUTPUT STREAM sFile TO VALUE(lDirName + "/" + LC(lFileName)) APPEND.
         PUT STREAM sFile UNFORMATTED "BWSNI:BNB=" lCli ",TDCL=0;" SKIP.
         OUTPUT STREAM sFile CLOSE.
      END.
   END.    
   ELSE IF CLIQueue.Command = "Delete" THEN DO:
      /* NOKIA ExCode */
      IF clique.ExCode = "DX200" THEN DO:
         ASSIGN lFileName = pResellerID + "del" + STRING(TODAY,"999999") +
                            sDX2Del + ".TXT".
         OUTPUT STREAM sFile TO VALUE(lDirName + "/" + LC(lFileName)) APPEND.
         PUT STREAM sFile UNFORMATTED lCli.
         PUT STREAM sFile CONTROL "~015" "~012".
         OUTPUT STREAM sFile CLOSE.
      END.
      /* ERICSSON ExCode */
      ELSE IF clique.ExCode = "AXE"   THEN DO:
         ASSIGN lFileName = "AXEDel" + STRING(TODAY,"999999") +
                            sAXEDel + ".TXT".
         OUTPUT STREAM sFile TO VALUE(lDirName + "/" + LC(lFileName)) APPEND.
         PUT STREAM sFile UNFORMATTED "BWSNE:BNB=" lCli ";" SKIP.
         OUTPUT STREAM sFile CLOSE.
      END.
   END.        

END.

IF lFileName = "" THEN DO:
   RUN clilog.p 
     (INPUT PROGRAM-NAME(1),
      INPUT "Could not find a CLI to put in ADD or Delete list." + 
            "Please contact a sysadm.",
      INPUT 0,       /* No ErrCode */
      INPUT NO,     /* NO = Date will NOT show in LOG */
      INPUT NO).    /* NO = EMail will NOT be Sent */

   pReturn = FALSE.
END.
ELSE pReturn = TRUE.





