/*------------------------------------------------------------
  MODULE .......: clireadf.p 
  FUNCTION .....: Read request File AND UPDATE db.
  APPLICATION ..: 
  AUTHOR .......: DW Orbil
  CREATED ......: 20.09.1999 
  MODIFIED .....: 02.11.1999    1.0f    * When applicable, leading 
                                          zero remains in the system
                  22.01.2000    1.0g    * SESSION:BATCH (twice)

                  05.09.2000 BY kl: ttCli, report double CLIs

  Version ......: M15
--------------------------------------------------------------*/

/* FOR batch RUN */
IF SESSION:BATCH THEN OUTPUT TO /dev/null.

{Syst/commali.i}  /*{Syst/commpaa.i} */
{Func/function.i}
{Func/timestamp.i}
{Func/tmsparam2.i}

{Mf/cliinlog.i "NEW" "request"}

DEFINE BUFFER buf-rsoper FOR rsoper.

DEFINE TEMP-TABLE ttCli NO-UNDO
   FIELD pref AS C
   FIELD CLI  AS C
   FIELD cust AS I
   FIELD Name AS C

   INDEX pref AS PRIMARY
      pref
      CLI.

DEFINE INPUT PARAM pCustNr      AS INTEGER   NO-UNDO.

DEFINE VARIABLE lCliFileName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCliFileDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCliSeparator   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError          AS INTEGER   NO-UNDO.
DEFINE VARIABLE lErrorCode      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lRecord         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustomerNo     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lResPref        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCli            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDigit          AS INTEGER   NO-UNDO.
DEFINE VARIABLE ix              AS INTEGER   NO-UNDO.

DEFINE VARIABLE cliListAdd      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lResellerID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOrder          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDetailCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lCreateDate     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCheck          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lDone           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lCreated        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lQueueMessg     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lNntable        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFileFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFiles          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lT1EPref        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCliDblFile     AS CHARACTER NO-UNDO.

DEFINE STREAM s1.
DEFINE STREAM s2.
DEFINE STREAM s3.

lT1EPref = fCParamC("Tele1Pref").

/*---- Functions ------------------------------------------*/

{Func/clivalif.i}

/*---- Start up the process -------------------------------*/

FOR EACH rsoper NO-LOCK WHERE
         rsoper.Reseller >= 1 AND
         rsoper.CustNum   = pCustNr,

   FIRST OperIndir NO-LOCK WHERE
         OperIndir.Operator = rsoper.Operator.

   RUN clilog.p 
     (INPUT PROGRAM-NAME(1),
      INPUT "Readprocessing starts...", 
      INPUT 0,      /* No ErrCode */
      INPUT YES,    /* YES = Date will show in LOG */
      INPUT NO).    /* NO  = No EMail will be Sent */

   ASSIGN
      /* Get the Name of the directory where the requestfiles are stored */
      lCliFileDir = fChkPath(rsoper.rs-dir) + "request/"
      /* Get the reseller's prefix. */
      lResPref = OperIndir.Prefix.

   IF rsoper.req-script NE "" THEN DO:
      /* RUN the script that Calls FOR the cli-file on the ftp-server. */
      IF OPSYS = "UNIX" THEN OS-COMMAND SILENT VALUE 
         ("( cd " + lCliFileDir + " ; " 
                  + rsoper.req-script + " > /dev/null )" ).
      ELSE OS-COMMAND SILENT VALUE(rsoper.req-script).
   END.

   /* Get the separator from TMSParam. */
   {Func/tmsparam.i CliFileSeparator RETURN}
   lCliSeparator = TMSParam.CharVal.

   IF OPSYS = "UNIX" THEN 
      INPUT STREAM s1 THROUGH VALUE ("ls -1 " + lCliFileDir).
   ELSE DO:
      OS-COMMAND SILENT VALUE("dir /b /on " + 
                               lCliFileDir  + 
                               " > CLIlist.txt ").    
      INPUT STREAM s1 FROM "clilist.txt".
   END.

   inputFile:
   REPEAT:  

      IMPORT STREAM s1 UNFORMATTED lCliFileName.

      IF SEARCH (lCliFileDir + lCliFileName) = ? THEN DO:
         RUN clilog.p 
           (INPUT PROGRAM-NAME(1),
            INPUT "The requestfile " + 
                   lCliFileDir +
                   lCliFileName + " not found.", 
            INPUT 0,       /* No ErrCode */
            INPUT NO,      /* NO = Date will NOT show in LOG */
            INPUT NO).     /* NO = EMail will NOT be Sent */
         /* Set the flag TO yes so that this MESSAGE won't show twice... */
         lFileFound = YES.    
         LEAVE.
      END.        

      RUN clilog.p 
        (INPUT PROGRAM-NAME(1),
         INPUT "File " + lCliFileName + " is being Processed.", 
         INPUT 0,      /* No ErrCode */
         INPUT NO,     /* YES = Date will show in LOG */
         INPUT NO).    /* NO = EMail will NOT be Sent */

      lFileFound = YES.

      /* Verify that the File has the correct FORMAT. */

      lError = ValFile(INPUT lCliFileDir + lCliFileName,
                       INPUT lCliSeparator,
                       INPUT rsoper.fileid).

      IF lError <> 0 THEN DO:

         RUN clilog.p 
           (INPUT PROGRAM-NAME(1),
            INPUT "File rejected.",
            INPUT lError,  
            INPUT NO,     /* Date will NOT show in LOG */     
            INPUT NO).    /* NO = EMail will NOT be Sent */    

         RUN clicrrej.p (INPUT lCliFileName,
                         INPUT lError).
         NEXT inputfile.

      END.

      INPUT STREAM s2 FROM VALUE(lCliFileDir + lCliFileName).

      REPEAT:

         /* Read the header */
         IMPORT STREAM s2 UNFORMATTED lRecord. 

         CASE ENTRY(1,lRecord,lCliSeparator):
            WHEN "HDR" THEN DO TRANSAction:

               lCustomerNo = rsoper.CustNum.

               ASSIGN 
                  lResellerID  = ENTRY(2,lRecord,lCliSeparator)
                  lOrder       = ENTRY(3,lRecord,lCliSeparator)
                  lDetailCount = INTEGER(ENTRY(4,lRecord,lCliSeparator))
                  lCreateDate  = ENTRY(5,lRecord,lCliSeparator).

               FIND FIRST CLIFile WHERE 
                          CLIFile.FileName = lCliFileName 
               EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE CLIFile THEN DO:
                  CREATE CLIFile.
                  ASSIGN CLIFile.FileName = lCliFileName.
               END.
               ELSE DO:
                  RUN clilog.p
                    (INPUT PROGRAM-NAME(1),
                     INPUT "File " + lCliFileName + 
                           " already updated in table CLIFile," +
                           " proceeds...",
                     INPUT 0,   /* No ErrCode */
                     INPUT NO,  /* NO = Date will NOT show in LOG */
                     INPUT NO). /* NO = EMail will NOT be Sent */
               END.

               ASSIGN 
                  CLIFile.OrderId           = lOrder
                  CLIFile.RsId        = lResellerId
                  CLIFile.DtlCount       = lDetailCount
                  CLIFile.InStamp = lCreateDate.

               LEAVE. 

            END.
            OTHERWISE DO:
               NEXT.      /* Ignore anything until header, "HDR" */
               /*********
               RUN clilog.p 
                 (INPUT PROGRAM-NAME(1),
                  INPUT "No header in requestfile. " +
                        "Error in validation code. " +
                        "Please contact a sysadm. "  +
                        "Processing stops.", 
                  INPUT 0,    /* No ErrCode */
                  INPUT YES,  /* NO = Date will NOT show in LOG */
                  INPUT NO).  /* NO = EMail will NOT be Sent */    
               RETURN.
                  *************/

            END.
         END. /* CASE */

      END. /* REPEAT: */

      ASSIGN
         lQueueMessg = NO
         lNntable    = NO.

      REPEAT:

         IMPORT STREAM s2 UNFORMATTED lRecord.

         CASE ENTRY(1,lRecord,lCliSeparator):
            WHEN "DTL" THEN DO TRANSAction:

               /* VALIDATE the CLI */

               /* FIRST check IF CLI contains anything but digits */
               lErrorCode = 0.
               DO ix = 1 TO LENGTH(ENTRY(4,lRecord,lCliSeparator)).
                  ASSIGN lDigit = 
                     INTEGER(SUBSTRING(ENTRY(4,lRecord,lCliSeparator),ix,1)) 
                  NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN DO:

                     lErrorCode = 1.
                     RUN clilog.p
                       (INPUT PROGRAM-NAME(1),
                        INPUT "Cli " +  ENTRY(4,lRecord,lCliSeparator),
                        INPUT lErrorCode, /* No ErrCode */
                        /* NO = Date will NOT show in LOG */
                        INPUT NO, 
                        /* NO = EMail will NOT be Sent */
                        INPUT NO).
                     LEAVE.
                  END.
               END.

               ASSIGN lCli = ENTRY(4,lRecord,lCliSeparator).

               /* Check the LENGTH of the CLI. 
                  No more than 16 digits is allowed. */
               IF LENGTH(lCli) > 16 THEN DO:
                  lErrorCode = 1.
                  RUN clilog.p
                    (INPUT PROGRAM-NAME(1),
                     INPUT "Cli " + lCli,
                     INPUT lErrorCode, /* No ErrCode */
                     /* NO = Date will NOT show in LOG */
                     INPUT NO,
                     /* NO = EMail will NOT be Sent */
                     INPUT NO).

               END.

               lCreated = NO.
               FIND FIRST CLIQueue WHERE 
                          CLIQueue.RsId = lResellerId AND
                          CLIQueue.order      = lOrder      AND
                          CLIQueue.CLI        = lCli 
               EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE CLIQueue THEN DO:
                  CREATE CLIQueue.
                  ASSIGN 
                     CLIQueue.RsId = lResellerID
                     CLIQueue.order      = lOrder
                     CLIQueue.CLI        = lCli
                     lCreated            = YES.

                  /* Hosting OR sharing reseller */
                  IF      rsoper.Reseller = 1 THEN CLIQueue.ExCode = "DX200".
                  ELSE IF rsoper.Reseller = 2 THEN CLIQueue.ExCode = "AXE".

               END.
               ELSE DO:
                  /* Show this MESSAGE only once. */
                  IF lQueueMessg = NO THEN DO:
                     RUN clilog.p 
                       (INPUT PROGRAM-NAME(1),
                        INPUT "Table CLIQueue already " +
                              "updated with OrderId " + lOrder + ".", 
                        INPUT 0,     /* No ErrCode */
                        INPUT NO,    /* NO = Date will NOT show in LOG */
                        INPUT NO).   /* NO = EMail will NOT be Sent */ 
                     lQueueMessg = YES.                                      
                  END.                                  
               END.

               ASSIGN 
                  CLIQueue.CreditLimit =
                    INTEGER(ENTRY(2,lRecord,lCliSeparator))
                  CLIQueue.Command     = ENTRY(3,lRecord,lCliSeparator)
                  CLIQueue.Date        = ENTRY(5,lRecord,lCliSeparator)
                  CLIQueue.ErrCode   = lErrorCode.

               IF lErrorCode = 0 THEN DO:

                  IF CLIQueue.Command = "Add" THEN DO:

                     FIND FIRST CLI WHERE
                                CLI.CLI = cliqueu.cli
                     NO-LOCK NO-ERROR.

                     IF AVAIL CLI THEN CASE rsoper.Reseller:
                        /* Sharing reseller */
                        WHEN 1 THEN DO:
                           IF CLI.CustNum = lCustomerNo THEN 
                              lErrorCode = 102.
                           ELSE DO:
                              FIND FIRST buf-rsoper WHERE
                                         buf-rsoper.CustNum = CLI.CustNum
                              NO-LOCK NO-ERROR.
                              IF AVAIL buf-rsoper THEN lErrorCode = 104.
                              ELSE lErrorCode = 101.
                           END.   

                           /* FOR reported double CLIs */
                           FIND FIRST Customer WHERE
                                      Customer.CustNum = CLI.CustNum
                           NO-LOCK NO-ERROR.
                           CREATE ttCli.
                           ASSIGN
                              ttCli.pref = lT1EPref
                              ttCli.CLI  = CLI.CLI
                              ttCli.cust = Customer.CustNum
                              ttCli.Name = Customer.CustName.

                           IF NOT lCreated THEN DO:
                              /* This MESSAGE shows only once in the logfile */
                              IF lNntable = NO THEN DO:
                                 RUN clilog.p 
                                   (INPUT PROGRAM-NAME(1),
                                    INPUT "Any valid CLI already "    +
                                          "updated in tables CLI " +
                                          "and CLISer, proceeds...", 
                                    INPUT 0,       /* No ErrCode */
                                    /* NO = Date will NOT show in LOG */
                                    INPUT NO, 
                                    /* NO = EMail will NOT be Sent */
                                    INPUT NO). 

                                 lNntable = YES.
                              END.                                          

                           END.
                        END.
                        /* Hosting reseller */
                        WHEN 2 THEN DO:
                           /* Check who owns the CLI (CLI) */
                           IF CLI.CustNum = lCustomerNo THEN DO:

                              /* IF Active THEN it exists in WhiteList */
                              IF CLI.Active = TRUE THEN DO:
                                 lErrorCode = 102.
                                 /* FOR reported double CLIs */
                                 FIND FIRST Customer WHERE
                                            Customer.CustNum = CLI.CustNum
                                 NO-LOCK NO-ERROR.
                                 CREATE ttCli.
                                 ASSIGN
                                    ttCli.pref = lT1EPref
                                    ttCli.CLI  = CLI.CLI
                                    ttCli.cust = Customer.CustNum
                                    ttCli.Name = Customer.CustName.
                              END.
                              /* CLI is removed from DX200 WhiteList */
                              ELSE DO:
                                 /* Remove old CLI definitions */
                                 FIND CURRENT CLI EXCLUSIVE-LOCK.
                                 DELETE CLI.
                                 FIND FIRST CLISer WHERE
                                            CLISer.CLIFrom = clique.cli  AND
                                            CLISer.CLITo = clique.cli  AND
                                            CLISer.CustNum = lCustomerNo
                                 EXCLUSIVE-LOCK NO-ERROR.
                                 IF AVAIL CLISer THEN DELETE CLISer.
                              END.
                           END.
                        END.
                     END.

                     IF lErrorCode = 0 THEN DO:
                        CASE rsoper.Reseller:
                           /* Sharing  reseller */
                           WHEN 1 THEN DO:
                              CREATE CLISer.
                              ASSIGN 
                                 CLISer.CustNum = lCustomerNo
                                 CLISer.CLIFrom = CLIQueue.CLI
                                 CLISer.CLITo = CLIQueue.CLI.
                               CREATE CLI.
                               ASSIGN 
                                  CLI.CustNum  = lCustomerNo
                                  CLI.CLI   = CLIQueue.CLI
                                  CLI.ValueLimit = CLIQueue.CreditLimit
                                  CLI.Ref   = CLIQueue.RsId
                                  CLI.Active  = YES.
                              /* CREATE a CPS request */
                              RUN pCreatePresel.
                           END.
                           /* Hosting reseller */
                           WHEN 2 THEN DO:
                              /* is this CLI marked FOR this reseller */
                              FIND FIRST CLIPref WHERE
                                         CLIPref.Pref = lResPref     AND
                                         CLIPref.CLI  = CLIQueue.CLI
                              NO-LOCK NO-ERROR.

                              IF AVAILABLE CLIPref THEN DO:
                                 lErrorCode = 102.
                                 /* FOR reported double CLIs */
                                 FIND FIRST Operator WHERE
                                            Operator.Operator = OperIndir.Operator
                                 NO-LOCK NO-ERROR.           
                                 IF AVAIL Operator THEN
                                    FIND FIRST Customer WHERE
                                               Customer.CustNum = Operator.CustNum
                                    NO-LOCK NO-ERROR.
                                 IF AVAIL Customer THEN DO:   
                                    CREATE ttCli.
                                    ASSIGN
                                       ttCli.pref = CLIPref.Pref
                                       ttCli.CLI  = CLIPref.CLI
                                       ttCli.cust = Customer.CustNum
                                       ttCli.Name = Customer.CustName.
                                 END.
                              END.
                              ELSE DO:
                                 CREATE CLIPref.
                                 ASSIGN 
                                    CLIPref.Pref  = lResPref
                                    CLIPref.CLI   = CLIQueue.CLI
                                    CLIPref.CLIId    = lCliFileName
                                    CLIPref.State = 0.
                              END.           

                           END. /* WHEN 2 */
                        END.  /* CASE ... */
                     END.   /* lErroCode = 0 */
                  END. /* ADD */
                  /* Carrier PreSelect */
                  ELSE IF CLIQueue.Command = "RACPS" THEN DO:
                     FIND FIRST CLI WHERE
                                CLI.CLI  = cliqueu.cli   AND
                                CLI.CustNum = rsoper.CustNum
                     NO-LOCK NO-ERROR.
                     IF NOT AVAIL CLI THEN lErrorCode = 301.
                     ELSE DO:
                        FIND FIRST Presel WHERE
                                   Presel.CustNum = rsoper.CustNum AND
                                   Presel.CLI  = clique.cli
                        EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAIL Presel THEN DO:
                           /* Sent TO Telia, NOT received back */
                           IF Presel.FileSeq1 NE 0 AND
                              Presel.FileSeq2 EQ 0 THEN lErrorCode = 302.
                           /* Same CLI - same Date */
                           ELSE IF Presel.AuthDate = DATE(
                                   INT(SUBSTR(CLIQueue.Date,5,2)), 
                                   INT(SUBSTR(CLIQueue.Date,7,2)), 
                                   INT(SUBSTR(CLIQueue.Date,1,4))) THEN
                              lErrorCode = 303.
                           /* Same CLI - NEW request */
                           ELSE DO:
                              DELETE Presel.
                              RUN pCreatePresel.
                              lErrorCode = 304.
                           END.
                        END.
                        /* CREATE a CPS request */
                        ELSE RUN pCreatePresel.
                     END.
                  END.
                  /* Deleting RepType 2 can mean DX200 ExCode */
                  ELSE IF CLIQueue.Command = "Delete" AND 
                          CLIQueue.ExCode  = "AXE"    THEN DO:
                     FIND FIRST CLI WHERE 
                                CLI.CLI  = CLIQueue.CLI AND
                                CLI.CustNum = rsoper.CustNum
                     NO-LOCK NO-ERROR.
                     IF AVAIL CLI THEN CLIQueue.ExCode = "DX200".
                  END.
                  ELSE lErrorCode = 5.
               END.   /* lErroCode = 0 */

               IF lErrorCode <> 0 THEN DO:
                  FIND FIRST CLIErrorCode WHERE 
                             CLIErrorCode.ErrCode = lErrorCode 
                  NO-LOCK NO-ERROR.
                  IF AVAILABLE CLIErrorCode THEN DO:
                     ASSIGN 
                        CLIQueue.ErrCode    = lErrorCode
                        CLIQueue.ErrMsg = CLIErrorCode.ErrMsg.
                     /* A MESSAGE OR an error */
                     IF CLIErrorCode.Action = "OK" THEN
                        CLIQueue.Result = "Ok".
                     ELSE
                        CLIQueue.Result = "Error".
                  END.
                  ELSE DO:
                     RUN clilog.p 
                       (INPUT PROGRAM-NAME(1),
                        INPUT "Errorcode is missing.", 
                        INPUT lErrorCode,  
                        INPUT NO,     /* NO = Date will NOT show in LOG */
                        INPUT NO).    /* NO = EMail will NOT be Sent */

                     ASSIGN 
                        CLIQueue.ErrCode    = lErrorCode
                        CLIQueue.ErrMsg = "Unknown error"
                        CLIQueue.Result       = "Error".
                  END.

               END. /* lErrorCode <> 0 */
               /* No error was found */
               ELSE ASSIGN
                  CLIQueue.ErrCode = lErrorCode
                  CLIQueue.Result    = "Ok".

            END. /* WHEN "DTL" */       
            WHEN "TLR" THEN ASSIGN 
               lDetailCount = INTEGER(ENTRY(4,lRecord,lCliSeparator)).
            OTHERWISE  NEXT.

         END.    /* END CASE */

      END.        /* END REPEAT */

      /* UPDATE CLIFile record WITH DtlCount */
      DO TRANSAction:       
         FIND FIRST CLIFile WHERE 
                    CLIFile.FileName = lCliFileName 
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE CLIFile THEN
            ASSIGN CLIFile.DtlCount = lDetailCount.
         ELSE DO:
            RUN clilog.p 
              (INPUT PROGRAM-NAME(1),
               INPUT "Record " + lCliFileName + 
                     " is missing in table CLIFile. Please contact a sysadm.",
               INPUT 0,      /* No ErrCode */
               INPUT NO,     /* NO = Date will NOT show in LOG */
               INPUT NO).    /* NO = EMail will NOT be Sent */
         END.

      END.

      IF CAN-FIND(FIRST CLIQueue WHERE 
                        CLIQueue.OrderId = lOrder AND 
                        CLIQueue.Command = "Add"  AND 
                        CLIQueue.ErrCode = 0)   OR
         CAN-FIND(FIRST CLIQueue WHERE 
                        CLIQueue.OrderId = lOrder   AND 
                        CLIQueue.Command = "Delete" AND 
                        CLIQueue.ErrCode = 0)  THEN DO:

         RUN clicrlis.p 
           (INPUT  lOrder,
            INPUT  lResellerId,
            OUTPUT lDone).

         IF NOT lDone THEN
           RUN clilog.p 
             (INPUT PROGRAM-NAME(1),
              INPUT "The whitelist updatefile was not created. " +
                    "Please contact a sysadm.",
              INPUT 0,      /* No ErrCode */
              INPUT NO,     /* NO = Date will NOT show in LOG */
              INPUT NO).    /* NO = EMail will NOT be Sent */
         ELSE 
            RUN clilog.p 
              (INPUT PROGRAM-NAME(1),
               INPUT "A whitelist updatefile has been created.",
               INPUT 0,      /* No ErrCode */
               INPUT NO,     /* NO = Date will NOT show in LOG */
               INPUT NO).    /* NO = EMail will NOT be Sent */
      END.
      /* When no WL requests were created:
         Response File is created rightaway */
      ELSE IF CAN-FIND(FIRST CLIQueue WHERE 
                             CLIQueue.OrderId = lOrder) THEN DO:
         RUN clicrres.p
           (INPUT  CLIQueue.OrderId,
            INPUT  rsoper.CustNum,
            OUTPUT lFiles,
            OUTPUT lDone).

         IF NOT lDone THEN
            RUN clilog.p 
              (INPUT PROGRAM-NAME(1),
               INPUT "Responsefile not created for OrderId " + 
                      CLIQueue.OrderId + ". Please contact a sysadm.", 
               INPUT 0,   /* No ErrCode */
               INPUT NO,          
               INPUT NO). /* NO = EMail will NOT be Sent */
      END.
      ELSE
         RUN clilog.p 
           (INPUT PROGRAM-NAME(1),
            INPUT "No new whitelist updatefile was created. " +
                  "No new valid CLI in requestfile.",
            INPUT 0,      /* No ErrCode */
            INPUT NO,     /* NO = Date will NOT show in LOG */
            INPUT NO).    /* NO = EMail will NOT be Sent */

      /* Save executed files in ZipCode FORMAT in a backup directory */
      RUN clisavef.p
        (INPUT lCliFileDir + lCliFileName, INPUT rsoper.CustNum).
      IF RETURN-VALUE <> "" THEN
         RUN clilog.p 
           (INPUT PROGRAM-NAME(1),
            INPUT "Could not store " + 
                  lCliFileDir  +
                  lCliFileName + " in backup directory" ,
            INPUT 0,      /* No ErrCode */
            INPUT NO,     /* NO = Date will NOT show in LOG */
            INPUT NO).    /* NO = EMail will NOT be Sent */
   END.

   IF NOT lFileFound THEN
      RUN clilog.p 
        (INPUT PROGRAM-NAME(1),
         INPUT "No requestfile found.",
         INPUT 0,      /* No ErrCode */
         INPUT NO,     /* NO = Date will NOT show in LOG */
         INPUT NO).    /* NO = EMail will NOT be Sent */

   RUN clilog.p 
     (INPUT PROGRAM-NAME(1),
      INPUT "Readprocessing done.",
      INPUT 0,       /* No ErrCode */
      INPUT YES,     /* NO = Date will NOT show in LOG */
      INPUT YES).    /* YES = EMail will be Sent */

END.

/* report double CLIs */
ASSIGN
   lCliDblFile = fChkPath(rsoper.rs-dir) + "log/"
   lCliDblFile = lCLiDblFile + "clidbl.log".

OUTPUT STREAM s3 TO VALUE(lCliDblFile) APPEND.
FOR EACH ttCli NO-LOCK:
   PUT STREAM s3 UNFORMATTED
      ttCli.CLI  chr(9)
      ttCli.pref chr(9)
      ttCli.cust chr(9)
      ttCli.Name chr(9)
      fMakeTS()  chr(10).
END.

/* Called when a NEW CPS request is created */
PROCEDURE pCreatePresel.

   CREATE Presel.
   ASSIGN
      Presel.CrStamp = fMakeTS() 
      Presel.ChStamp = Presel.CrStamp 
      Presel.CustNum   = rsoper.CustNum 
      Presel.PsType   = rsoper.pstype 
      Presel.CLI    = CLIQueue.CLI 
      Presel.AuthDate =  
         DATE(INT(SUBSTR(CLIQueue.Date,5,2)), 
              INT(SUBSTR(CLIQueue.Date,7,2)), 
              INT(SUBSTR(CLIQueue.Date,1,4))).
      /*        
      Presel.Orderer  = lCliFileName.
      */
END PROCEDURE.

