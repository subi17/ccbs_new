/*------------------------------------------------------------
  MODULE .......: cliswlog.p
  FUNCTION .....: Reads AND saves the Result from the switch's 
                  logfile, into the CLIQueue.
  APPLICATION ..: 
  AUTHOR .......: DW Orbil
  CREATED ......: 20.09.1999 
  MODIFIED .....: 20.10.1999 1.0c * Switchlog is always LC
                  28.10.1999 1.0d Fetch search STRING 1&2 from TMSParam. (PG)
                  01.11.1999 1.0e Removes Processed files AND store them in
                               backup dir (PG)  
                  02.11.1999 1.0f Add sign (Zero in Sweden) TO CLI.
                                  Sign is stored in tmsparam.initialCliSign.  
  Version ......: M15
--------------------------------------------------------------*/

{Syst/commali.i} 
{Func/function.i}

DEFINE STREAM sLog.

DEFINE VARIABLE response       AS CHARACTER NO-UNDO.
DEFINE VARIABLE line           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCli           AS CHARACTER NO-UNDO.
DEFINE VARIABLE FileDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE FileName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE searchstring1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE searchstring2  AS CHARACTER NO-UNDO.
DEFINE VARIABLE axestring1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE axestring2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cli-pos-start  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cli-pos-end    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lresult        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDone          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lprocessed     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lInitSign      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFiles         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMsg           AS CHARACTER NO-UNDO.
DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
DEFINE VARIABLE wlrecid        AS REC       NO-UNDO.


DEFINE TEMP-TABLE wlresp NO-UNDO
   FIELD ROW AS C.

DEFINE BUFFER bufcliwl FOR CLIWL.

{Mf/cliinlog.i "NEW" "swlog"}

RUN Mf/clilog.p 
  (INPUT PROGRAM-NAME(1),
   INPUT "Switchlogprocessing starts...", 
   INPUT 0,    /* No ErrCode */
   INPUT YES,  /* YES = Date will show in LOG */
   INPUT NO).  /* NO = EMail will NOT be Sent */

/* WhiteList LOG directory */
{Func/tmsparam.i SwitchLogFileDir RETURN}
FileDir = fChkPath(TMSParam.CharVal).  /* Directory Name */

/* Search STRING TO seach FOR a specific STRING in SW-log File */
{Func/tmsparam.i SwLogSearch1 RETURN}
searchstring1 = TMSParam.CharVal. 

/* Seach STRING TO seach FOR a specific sign in SW-log File */
{Func/tmsparam.i SwLogSearch2 RETURN}
searchstring2 = TMSParam.CharVal. 

/* Search STRING TO seach FOR a specific STRING in AXE-log File */
{Func/tmsparam.i AxeLogSearch1 RETURN}
axestring1 = TMSParam.CharVal. 

/* Seach STRING TO seach FOR a specific sign in AXE-log File */
{Func/tmsparam.i AxeLogSearch2 RETURN}
axestring2 = TMSParam.CharVal. 

{Func/tmsparam.i InitialCliSign RETURN}.
lInitSign = CharVal.

FOR EACH CLIWL NO-LOCK WHERE 
         CLIWL.Processed = NO.

   /* FIND reseller */      
   FIND FIRST rsoper WHERE
              rsoper.fileid = substr(CLIWL.FileName,1,length(rsoper.fileid))
   NO-LOCK NO-ERROR.
   IF NOT AVAIL rsoper THEN DO:
      FIND FIRST CLIPref WHERE
         INDEX(CLIPref.CLIId,CLIWL.OrderId) > 0
      NO-LOCK NO-ERROR.
      IF AVAIL CLIPref THEN
         FIND FIRST rsoper WHERE
            CLIPref.CLIId BEGINS rsoper.fileid
         NO-LOCK NO-ERROR.
   END.

   /* Table where operators prefix is */      
   IF AVAIL rsoper THEN
      FIND FIRST OperIndir WHERE
                 OperIndir.Operator = rsoper.Operator
      NO-LOCK NO-ERROR.

   IF NOT AVAIL OperIndir THEN DO:
      RUN Mf/clilog.p 
        (INPUT PROGRAM-NAME(1),
         INPUT "Operator prefix definition is missing.",
         INPUT 0,        /* No ErrCode */
         INPUT NO,       /* YES = Date will show in LOG */
         INPUT NO).      /* NO = EMail will NOT be Sent */
      NEXT.
   END.

   ASSIGN
      lprocessed = YES
      FileName   = LC(SUBSTRING(CLIWL.FileName,1,LENGTH(CLIWL.FileName) - 4)).

   IF SEARCH(FileDir + FileName + ".log") = ? THEN DO:
      RUN Mf/clilog.p 
        (INPUT PROGRAM-NAME(1),
         INPUT "The File " + FileDir + 
                FileName + ".log" + " not created yet.", 
         INPUT 0,        /* No ErrCode */
         INPUT NO,       /* YES = Date will show in LOG */
         INPUT NO).      /* NO = EMail will NOT be Sent */
      NEXT.
   END.        

   RUN Mf/clilog.p 
     (INPUT PROGRAM-NAME(1),
      INPUT "The File " + FileDir + 
             FileName + ".log" + " is being Processed.", 
      INPUT 0,      /* No ErrCode */
      INPUT NO,     /* YES = Date will show in LOG */
      INPUT NO).    /* NO = EMail will NOT be Sent */ 

   INPUT STREAM sLog FROM VALUE(FileDir + FileName + ".log").

   REPEAT:
      IMPORT STREAM sLog UNFORMATTED response.
      CASE CLIWL.ExCode:
         /* DX200: 1 ROW / CLI */
         WHEN "DX200" THEN DO:
            CREATE wlresp.
            ASSIGN wlresp.row = response.
         END.
         /* AXE: many rows / CLI */
         WHEN "AXE" THEN DO:
            IF SUBSTR(TRIM(response),1,4) = "<BWS" THEN DO:
               CREATE wlresp.
               ASSIGN 
                  wlresp.row = TRIM(response)
                  wlrecid    = RECID(wlresp).
            END.
            ELSE IF SUBSTR(TRIM(response),1,1) = "<" THEN LEAVE.
            ELSE IF TRIM(response) NE "" THEN DO:
               FIND FIRST wlresp WHERE
                    RECID(wlresp) = wlrecid
               NO-ERROR.
               IF AVAIL wlresp THEN ASSIGN
                  wlresp.row = wlresp.row + TRIM(response) + ";".
            END.
         END.
       END.
   END.

   FOR EACH wlresp:   

      /* OLD WAY ...................................................
      /* different ExCode, different File FORMAT */
      CASE CLIWL.ExCode:
         /* DX200: 1 ROW / CLI */
         WHEN "DX200" THEN IMPORT STREAM sLog UNFORMATTED response.

         /* AXE: many rows / CLI */
         WHEN "AXE" THEN DO:
            response = "".
            IMPORT STREAM sLog UNFORMATTED line.
            /* ADD File response */
            IF INDEX(FileName,"add") > 0 THEN DO WHILE line NE "".
               ASSIGN
                  line     = TRIM(line)
                  response = response + line.
               /* add ";" AS a column separator */
               IF SUBSTR(response,LENGTH(response),1) NE ";" THEN 
                  response = response + ";".
               IMPORT STREAM sLog UNFORMATTED line.
            END.
            /* DELETE response File */
            ELSE IF INDEX(FileName,"del") > 0 THEN
            DO WHILE SUBSTR(TRIM(line),1,1) NE "(".
               IF line NE "" THEN DO:
                  ASSIGN
                     line     = TRIM(line)
                     response = response + line.
                  /* add ";" AS a column separator */
                  IF SUBSTR(response,LENGTH(response),1) NE ";" THEN 
                     response = response + ";".
               END.
               IMPORT STREAM sLog UNFORMATTED line.
            END.
         END.
      END.
      ...................................................... */


      /* FIND out on what position the CLI starts AND ends 
         (using the searchstring1 AND 2)  */
      CASE CLIWL.ExCode:
         /* NOKIA */
         WHEN "DX200" THEN
            ASSIGN
               cli-pos-start = 
                  INDEX(wlresp.row,searchstring1) + LENGTH(searchstring1)
               cli-pos-end = 
                  INDEX(wlresp.row,searchstring2,cli-pos-start)
               lCli = lInitSign + 
                  SUBSTR(wlresp.row,cli-pos-start,cli-pos-end - cli-pos-start)
               lresult = TRIM(SUBSTRING(wlresp.row,cli-pos-end + 
                         1,LENGTH(wlresp.row))).
         /* ERICSSON */
         WHEN "AXE" THEN DO:
            lCli = "".
            i = INDEX(wlresp.row,"BNB=") + 4.
            DO WHILE SUBSTR(wlresp.row,i,1) NE ";":
               ASSIGN
                  lCli = lCli + SUBSTR(wlresp.row,i,1)
                  i    = i + 1.
            END.
            lResult = ENTRY(2,wlresp.row,";").
         END.
      END. /* CASE */

      IF SUBSTR(lCLi,1,1) NE "0" THEN lCli = "0" + lCli.

      /* Different switches - different commands */
      IF (CLIWL.ExCode = "AXE"   AND index(lResult,"EXECUTED") > 0)    OR 
         (CLIWL.ExCode = "DX200" AND (lresult BEGINS "COMMAND EXECUTED" OR 
                                      lresult BEGINS "SUBSCRIBER"))     THEN DO:

         FIND FIRST cliqueu WHERE 
                    CLIQueue.OrderId = CLIWL.OrderId AND 
                    CLIQueue.CLI     = lCli 
         EXCLUSIVE-LOCK NO-ERROR.

         IF AVAILABLE CLIQueue THEN DO:

            ASSIGN 
               CLIQueue.ErrCode = 0
               CLIQueue.Result    = "Ok".

            CASE CLIQueue.Command:

               WHEN "ADD" THEN DO:
                  CASE rsoper.Reseller:
                     WHEN 1 THEN DO:
                        FIND FIRST CLIPref WHERE
                                   CLIPref.Pref = OperIndir.Prefix AND
                                   CLIPref.CLI  = lCli
                        EXCLUSIVE-LOCK NO-ERROR.
                        /* CLI is in the WhiteList */
                        IF AVAIL CLIPref THEN CLIPref.State = 1.
                     END.
                  END.
               END.

               WHEN "DELETE" THEN DO:
                  CASE rsoper.Reseller:
                     WHEN 1 THEN DO:

                        /* Prevent future CPS requests */
                        FIND FIRST Presel WHERE
                                   Presel.CustNum = rsoper.CustNum AND
                                   Presel.CLI  = cliqueu.cli
                        EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAILABLE Presel THEN Presel.ChStamp = 0.

                        /* Mark CLI AS NOT ACTIVE */
                        FIND FIRST CLI WHERE 
                                   CLI.CLI  = CLIQueue.CLI AND
                                   CLI.CustNum = rsoper.CustNum
                        EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAILABLE CLI THEN CLI.Active = FALSE.

                        /*******************
                        IF AVAILABLE CLI THEN DELETE CLI.

                        FIND FIRST CLISer WHERE 
                                   CLISer.CustNum = rsoper.CustNum AND 
                                   CLISer.CLIFrom = CLIQueue.CLI  AND 
                                   CLISer.CLITo = CLIQueue.CLI 
                        EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAILABLE CLISer THEN DELETE CLISer.                
                        *******************/

                     END. /* WHEN 1 */
                     WHEN 2 THEN DO:
                        FIND FIRST CLIPref WHERE
                                   CLIPref.Pref = OperIndir.Prefix AND
                                   CLIPref.CLI  = lCli
                        EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAIL CLIPref THEN DELETE CLIPref.
                     END. /* WHEN 2 */

                  END. /* CASE rsoper.Reseller */

               END. /* when CLIQueue.Command = "Delete"... */

            END. /* CASE cliqueu.command */

         END.

      END.            
      /* WhiteList was NOT updated */  
      ELSE DO:
         FIND FIRST cliqueu WHERE 
                    CLIQueue.OrderId = CLIWL.OrderId AND
                    CLIQueue.CLI     = lCli 
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE CLIQueue THEN DO:
             ASSIGN 
                CLIQueue.ErrCode = 201
                CLIQueue.Result    = "Error".
            /* IF ExCode didn't approve adding */
            IF CLIQueue.Command = "ADD" THEN DO:
               FIND FIRST CLIPref WHERE 
                          CLIPref.Pref = OperIndir.Prefix
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL CLIPref THEN DELETE CLIPref.
            END.
         END.
      END.

      DELETE wlresp.

   END.        

   FIND FIRST bufcliwl WHERE 
        ROWID(bufcliwl) = ROWID(CLIWL) 
   EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE bufcliwl THEN 
      ASSIGN bufCLIWL.Processed = YES.

   FIND CURRENT bufcliwl NO-LOCK.

   IF NOT CAN-FIND(FIRST bufcliwl WHERE 
                         bufCLIWL.Processed = NO AND
                         bufCLIWL.OrderId   = CLIWL.OrderId) THEN DO:

      RUN Mf/clicrres.p
        (INPUT  CLIWL.OrderId,
         INPUT  rsoper.CustNum,
         OUTPUT lFiles,
         OUTPUT lDone).

      IF NOT lDone THEN
         RUN Mf/clilog.p 
           (INPUT PROGRAM-NAME(1),
            INPUT "Responsefile not created for OrderId " + 
                   CLIWL.OrderId + ". Please contact a sysadm.", 
            INPUT 0,   /* No ErrCode */
            INPUT NO,          
            INPUT NO). /* NO = EMail will NOT be Sent */

   END.
   ELSE DO:
      RUN Mf/clilog.p 
        (INPUT PROGRAM-NAME(1),
         INPUT "No responsefile created. " +
               "The order with OrderId "   + CLIWL.OrderId +
               "still expects logfiles from the ExCode.",
         INPUT 0,   /* No ErrCode */
         INPUT NO,
         INPUT NO).  /* NO = EMail will NOT be Sent */
      IF lFiles NE "" THEN lMsg = lMsg + chr(10) + lFiles.
   END.

   /* Save executed files (both .log AND .txt) 
      in ZipCode FORMAT in a backup directory */
   RUN Mf/clisavef.p
     (INPUT FileDir + FileName + ".log",
      INPUT rsoper.CustNum).

   IF RETURN-VALUE <> "" THEN
      RUN Mf/clilog.p 
        (INPUT PROGRAM-NAME(1),
         INPUT "Could not store " + FileDir + 
                FileName + ".log" + " in backup directory" ,
         INPUT 0,   /* No ErrCode */
         INPUT NO,  /* NO = Date will NOT show in LOG */
         INPUT NO). /* NO = EMail will NOT be Sent */

   RUN Mf/clisavef.p 
     (INPUT FileDir + FileName + ".txt",
      INPUT rsoper.CustNum).

   IF RETURN-VALUE <> "" THEN
      RUN Mf/clilog.p 
        (INPUT PROGRAM-NAME(1),
         INPUT "Could not store " + FileDir + 
                FileName + ".txt" + " in backup directory" ,
         INPUT 0,   /* No ErrCode */
         INPUT NO,  /* NO = Date will NOT show in LOG */
         INPUT NO). /* NO = EMail will NOT be Sent */

END.    

IF NOT lprocessed THEN
  RUN Mf/clilog.p 
    (INPUT PROGRAM-NAME(1),
     INPUT "No logfile expected, switchlogprocessing done.", 
     INPUT 0,     /* No ErrCode */
     INPUT YES,   /* YES = Date will show in LOG */
     INPUT YES).  /* YES = EMail will be Sent */ 
ELSE
  RUN Mf/clilog.p 
    (INPUT PROGRAM-NAME(1),
     INPUT "Switchlogprocessing done.", 
     INPUT 0,    /* No ErrCode */
     INPUT YES,  /* YES = Date will show in LOG */
     INPUT YES). /* YES = EMail will be Sent */

IF NOT SESSION:BATCH THEN MESSAGE " DONE " VIEW-AS ALERT-BOX.

