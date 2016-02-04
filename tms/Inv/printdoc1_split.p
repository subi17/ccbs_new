/* ----------------------------------------------------------------------------
  MODULE .......: printdoc1_split.p
  FUNCTION .....: Split invoices to multiple doc1-files
  APPLICATION ..: TMS
  CREATED ......: 21.02.08/aam
  CHANGED ......: 
  Version ......: TMS
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/files.i}
{Syst/funcrunprocess_update.i}
{Syst/funcrun_replica.i}

DEF INPUT  PARAMETER idaInvDate      AS DATE NO-UNDO.
DEF INPUT  PARAMETER ilOnlyNew       AS LOG  NO-UNDO.
DEF INPUT  PARAMETER ilInvType       AS LOG  NO-UNDO.
DEF INPUT  PARAMETER icPrintHouse    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiBatchQty      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFileType      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ilDBWrite       AS LOG  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRExecID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdInterval   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUseReplica    AS INT  NO-UNDO.
DEF INPUT  PARAMETER ilChkInvNumbers AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER ocFileList      AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiInvCnt        AS INT  NO-UNDO. 

DEF VAR liTotal      AS INT    NO-UNDO.
DEF VAR lcSplitDir   AS CHAR   NO-UNDO.
DEF VAR liInvCnt     AS INT    NO-UNDO.
DEF VAR liBatchCnt   AS INT    NO-UNDO.
DEF VAR liCallQty    AS INT    NO-UNDO.
DEF VAR lcFile       AS CHAR   NO-UNDO.
DEF VAR lhCustomer   AS HANDLE NO-UNDO. 
DEF VAR lhField      AS HANDLE NO-UNDO.
DEF VAR lcFieldList  AS CHAR   NO-UNDO.
DEF VAR liInvType    AS INT    NO-UNDO.
DEF VAR lcStarted    AS CHAR   NO-UNDO.
DEF VAR lcCurrent    AS CHAR   NO-UNDO.
DEF VAR ldCheckRun   AS DEC    NO-UNDO.
DEF VAR lcDefaultPHouse AS CHAR NO-UNDO.
DEF VAR lcPickPHouse    AS CHAR NO-UNDO.
DEF VAR liPHouseCnt     AS INT  NO-UNDO.
DEF VAR ldThisRun       AS DEC  NO-UNDO.
DEF VAR lcError         AS CHAR NO-UNDO.
DEF VAR liHandled       AS INT  NO-UNDO.
DEF VAR liCnt           AS INT  NO-UNDO.
DEF VAR llOk            AS LOG  NO-UNDO.
DEF VAR lcActionID      AS CHAR NO-UNDO.
DEF VAR lcCheckFiles    AS CHAR NO-UNDO.
DEF VAR lcActionDir     AS CHAR NO-UNDO.
DEF VAR lcActionFile    AS CHAR NO-UNDO.
DEF VAR lcReplica       AS CHAR NO-UNDO.

DEF STREAM sFile.
DEF STREAM sAction.

DEF BUFFER bActionLog FOR ActionLog.

DEF TEMP-TABLE ttInv NO-UNDO
   FIELD InvNum     AS INT
   FIELD ZipCode    AS CHAR
   FIELD CallQty    AS INT
   FIELD PrintHouse AS CHAR
   INDEX ZipCode PrintHouse ZipCode CallQty.

DEF TEMP-TABLE ttPick NO-UNDO
   FIELD PrintHouse AS CHAR
   FIELD FieldName  AS CHAR
   FIELD FieldValue AS CHAR
   INDEX FieldValue FieldName FieldValue. 
   
DEF TEMP-TABLE ttField NO-UNDO
   FIELD FieldName AS CHAR
   INDEX FieldName FieldName.

DEF TEMP-TABLE ttPrintHouse NO-UNDO
   FIELD PrintHouse AS CHAR
   FIELD MaxPortion AS INT
   FIELD PortionPool AS LOG 
   FIELD InvQty AS INT
   INDEX PrintHouse PrintHouse.
   
FORM
   SKIP(1) 
   "Waiting invoice runs to finish ..       " AT 10 SKIP(1) 
   lcStarted     COLON 15 FORMAT "X(20)"         LABEL "Started" 
   lcCurrent     COLON 15 FORMAT "X(20)"         LABEL "Latest Loop" 
   SKIP(1)
WITH CENTERED ROW 8 SIDE-LABELS OVERLAY
   TITLE " COLLECT INVOICES FOR PRINTING " FRAME fWait.


/***** Main start ********/

RUN pInitialize.
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

RUN pGetPrintHouses.

RUN pMarkStarted.
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

RUN pWaitForInvoiceRun.
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

IF ilChkInvNumbers THEN DO:
   RUN pCheckNumbering.
   IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.
END.

RUN pCollectInvoices.
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

RUN pDivideIntoGroups.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

RUN pMarkFinished.

IF NOT ilDBWrite THEN OUTPUT STREAM sAction CLOSE.


/****** Main end *****/


PROCEDURE pInitialize:

   IF idaInvDate = ? OR iiBatchQty = 0 THEN DO:
      RETURN "ERROR:Nothing to do".
   END.

   IF iiUseReplica > 0 THEN DO:
      lcReplica = fInitReplicaSettings(iiUseReplica,
                                       INPUT-OUTPUT iiBatchQty).
      IF lcReplica BEGINS "ERROR" THEN RETURN lcReplica.   
   END.
   
   IF NOT ilDBWrite THEN DO:

      IF iiFRProcessID > 0 THEN RETURN 
         "ERROR:Function execution needs to write to db".
   
      lcActionDir = fCParamC("SplitActionDir").
      IF lcActionDir = ? OR lcActionDir = "" THEN lcActionDir = "/tmp".
      lcActionFile = lcActionDir + "/" + icFileType + "_split_" +
                     STRING(YEAR(TODAY),"9999") +
                     STRING(MONTH(TODAY),"99") +
                     STRING(DAY(TODAY),"99") + "_" + 
                     REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".txt".
      OUTPUT STREAM sAction TO VALUE(lcActionFile).
   END.

   IF iiFRProcessID = 0 THEN DO:
      IF icFileType BEGINS "XML" THEN ASSIGN
         lcSplitDir   = fCParamC("SplitInvXMLDir")
         lcCheckFiles = "invxml*"
         lcActionID   = "SplitInvXML".
   
      ELSE ASSIGN
         lcSplitDir   = fCParamC("SplitDoc1Dir")
         lcCheckFiles = "doc1*"
         lcActionID   = "SplitDoc1".

      IF lcSplitDir = ? OR lcSplitDir = "" THEN lcSplitDir = "/tmp".

      /* check that there aren't any old files left */
      IF fHasDirectoryFiles(lcSplitDir,lcCheckFiles) THEN 
         RETURN "ERROR:Directory " + lcSplitDir + " already contains files".
   END.
   ELSE lcActionID = "SplitInvPrint".
                                        
   /* default printing house */
   lcDefaultPHouse = fCParamC("Doc1PrintHouse").
   IF lcDefaultPHouse > "" THEN DO:
      CREATE ttPrintHouse.
      ASSIGN 
         ttPrintHouse.PrintHouse = lcDefaultPHouse
         ttPrintHouse.PortionPool = TRUE.
   END.

   lhCustomer = BUFFER Customer:HANDLE.

   /* get possible field names */
   DO liInvCnt = 1 TO lhCustomer:NUM-FIELDS:
      lhField = lhCustomer:BUFFER-FIELD(liInvCnt).
      lcFieldList = lcFieldList + (IF liInvCnt > 1 THEN "," ELSE "") + 
                    lhField:NAME.
   END.

   IF ilInvType = FALSE 
   THEN liInvType = 99.
   ELSE liInvType = 1.

   ASSIGN
      liInvCnt   = 0 
      ldThisRun  = fMakeTS()
      lcStarted  = fTS2HMS(ldThisRun)
      ldCheckRun = fOffSet(ldThisRun,-72).
      
   RETURN "".    

END PROCEDURE.

PROCEDURE pGetPrintHouses:

   DEF VAR liPortion AS INT  NO-UNDO.
   
   
   /* selection criteria */
   FOR EACH PrintHouseConf NO-LOCK WHERE
            PrintHouseConf.Brand     = gcBrand     AND
            PrintHouseConf.Report    = "Invoice"   AND
            PrintHouseConf.ToDate   >= TODAY      AND
            PrintHouseConf.FromDate <= TODAY:

      IF icPrintHouse > "" AND 
         LOOKUP(PrintHouseConf.PrintHouse,icPrintHouse) = 0
      THEN NEXT.
   
      IF LOOKUP(PrintHouseConf.FieldName,lcFieldList) = 0 THEN NEXT.
   
      CREATE ttPick.
      ASSIGN
         ttPick.PrintHouse = PrintHouseConf.PrintHouse
         ttPick.FieldName  = PrintHouseConf.FieldName
         ttPick.FieldValue = PrintHouseConf.KeyValue.
      
      IF NOT CAN-FIND(FIRST ttField WHERE ttField.FieldName = ttPick.FieldName)
      THEN DO:
         CREATE ttField.
         ttField.FieldName = ttPick.FieldName.
      END.
   
      IF NOT CAN-FIND(FIRST ttPrintHouse WHERE 
                         ttPrintHouse.PrintHouse = PrintHouseConf.PrintHouse)
      THEN DO:
         CREATE ttPrintHouse.
         ASSIGN 
            ttPrintHouse.PrintHouse = PrintHouseConf.PrintHouse
            ttPrintHouse.PortionPool = TRUE.
      END.
   
   END.

   IF icFileType = "XMLSEP" THEN DO:
      CREATE ttPrintHouse.
      ASSIGN 
         ttPrintHouse.PrintHouse = "QVANTEL"
         ttPrintHouse.PortionPool = FALSE.
   END.

   /* separate printing for customers with delivery type 10, 
      'no paper invoice' */
   ELSE IF NOT icFileType BEGINS "XML" THEN DO:
      CREATE ttPrintHouse.
      ASSIGN 
         ttPrintHouse.PrintHouse = "NOPAPER"
         ttPrintHouse.PortionPool = FALSE.
   END.
      
   /* if no printhouse configuration done then create one field entry to 
      enable collecting */
   IF NOT CAN-FIND(FIRST ttField) THEN DO:
      CREATE ttField.
      ttField.FieldName = ENTRY(1,lcFieldList).
   END.

   /* portions */
   IF icFileType NE "XMLSEP" THEN
   FOR EACH ttPrintHouse WHERE 
            ttPrintHouse.PortionPool = TRUE AND
            ttPrintHouse.PrintHouse NE lcDefaultPHouse:
      liPortion = fCparamI("Doc1PHPortion" + ttPrintHouse.PrintHouse).
      IF liPortion NE 0 AND liPortion NE ? THEN 
         ttPrintHouse.MaxPortion = liPortion.
   END.

   RETURN "".
   
END PROCEDURE.

PROCEDURE pMarkStarted:

   /* check that there isn't already another run for the same purpose */
   IF ilDBWrite THEN DO: 

      IF CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
                        ActionLog.Brand        = gcBrand     AND    
                        ActionLog.ActionID     = lcActionID  AND
                        ActionLog.ActionTS    >= ldCheckRun  AND
                        ActionLog.ActionStatus = 0           AND
                        ActionLog.FromDate     = idaInvDate)
      THEN DO:
         lcError = "ERROR:Another split run has already been started".
         IF NOT SESSION:BATCH THEN 
            MESSAGE lcError VIEW-AS ALERT-BOX INFORMATION.
         RETURN lcError.
      END.

      /* mark this run started */
      DO TRANS:
         CREATE ActionLog.
   
         ASSIGN
            ActionLog.ActionTS     = ldThisRun
            ActionLog.Brand        = gcBrand
            ActionLog.TableName    = "Invoice"
            ActionLog.KeyValue     = icFileType
            ActionLog.UserCode     = katun
            ActionLog.ActionID     = lcActionID
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                     MONTH(TODAY)
            ActionLog.FromDate     = idaInvDate
            ActionLog.ToDate       = idaInvDate
            ActionLog.ActionStatus = 0.
         RELEASE ActionLog.   
      END.
   END.

   ELSE DO:
      PUT STREAM sAction UNFORMATTED
         icFileType "|"
         katun      "|"
         lcActionID "|"
         idaInvDate "|"
         "Started"  "|"
         STRING(TIME,"hh:mm:ss")
         SKIP.
   END.

   RETURN "".

END PROCEDURE.

PROCEDURE pWaitForInvoiceRun:

   /* check that all invoice runs are finished */
   DO WHILE TRUE
   ON STOP UNDO, RETRY
   ON QUIT UNDO, RETRY:

      IF RETRY THEN DO:
         RUN pMarkFinished.
         RETURN "INFORMATION:Billing run ongoing".
      END.
 
      IF NOT CAN-FIND(FIRST ActionLog USE-INDEX ActionID WHERE
                            ActionLog.Brand        = gcBrand    AND    
                            ActionLog.ActionID     = "BillRun"  AND
                            ActionLog.ActionTS    >= ldCheckRun AND
                            ActionLog.TableName    = "Invoice"  AND
                            ActionLog.ActionStatus = 0)
      THEN LEAVE.

      lcCurrent = fTS2HMS(fMakeTS()).
     
      IF SESSION:BATCH THEN DO:
         RUN pMarkFinished.
         RETURN "INFORMATION:Billing run ongoing".
      END.
      
      PAUSE 0.
      DISPLAY lcStarted lcCurrent WITH FRAME fWait.
   
      PUT SCREEN ROW 22 COL 2 "F8 TO QUIT".
 
      READKEY PAUSE 60.
    
      IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO:
         RUN pMarkFinished.
         RETURN "INFORMATION:Billing run ongoing".
      END.
   
      PUT SCREEN ROW 22 COL 2 FILL(" ",20).

   END.

   HIDE FRAME fWait NO-PAUSE.

   RETURN "".

END PROCEDURE.

PROCEDURE pCheckNumbering:

   /* check that invoice numbering is ok */
   IF liInvType NE 99 THEN DO:

      /* give numbers to those that haven't yet got them */
      IF ilDBWrite THEN DO:

         IF NOT SESSION:BATCH THEN 
            PUT SCREEN ROW 22 COL 2 "Run invoice numbering ..". 
         RUN Inv/invoice_extinvid.p(idaInvDate,
                                liInvType,
                                2,   
                                0,
                                0,
                                OUTPUT liHandled).
      END.
   
      IF NOT SESSION:BATCH THEN 
         PUT SCREEN ROW 22 COL 2 "Check invoice numbering ..". 
   
      /* check */
      RUN Inv/invoice_extinvid.p(idaInvDate,
                             liInvType,
                             0,   
                             0,
                             0,
                             OUTPUT liHandled).
      IF NOT SESSION:BATCH THEN 
         PUT SCREEN ROW 22 COL 2 FILL(" ",20).
                        
      IF RETURN-VALUE BEGINS "ERROR:" THEN DO:
      
         llOk = FALSE.
         
         IF NOT SESSION:BATCH THEN DO:
            MESSAGE "There are inconsistencies in invoice IDs." SKIP
                    "Do You still want to create the invoice file?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            SET llOk.
         END.
         
         IF NOT llOk THEN DO:
            RUN pMarkFinished.
            RETURN "ERROR:Problem with external invoice IDs".
         END.   
      END.
   END.

   RETURN "".

END PROCEDURE.

PROCEDURE pCollectInvoices:
 
   DEF VAR liPortionTotal AS INT  NO-UNDO.
   DEF VAR liPortionQty   AS INT  NO-UNDO.
   
   DEF BUFFER bDefaultHouse FOR ttPrintHouse.
   
   InvoiceSelect:
   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
            Invoice.Brand      = gcBrand    AND
            Invoice.InvDate    = idaInvDate AND
            Invoice.InvType    = liInvType,
      FIRST Customer OF Invoice NO-LOCK:

      IF ilOnlyNew AND Invoice.PrintState > 0 THEN NEXT.

      lcPickPHouse = "".
   
      IF icFileType = "XMLSEP" THEN 
         lcPickPHouse = "QVANTEL".
   
      ELSE DO:
         /* no paper printing */
         IF Invoice.DelType > 1 THEN lcPickPHouse = "NOPAPER".
      
         ELSE 
         FOR EACH ttField: 

            lhField = lhCustomer:BUFFER-FIELD(ttField.FieldName).
      
            FIND FIRST ttPick WHERE
                       ttPick.FieldName  = ttField.FieldName AND
                       ttPick.FieldValue = lhField:BUFFER-VALUE NO-ERROR.
                 
            /* all customers that are not chosen for a particular printhouse 
               are directed to the default printhouse */
            IF NOT AVAILABLE ttPick THEN lcPickPHouse = lcDefaultPHouse.
       
            ELSE lcPickPHouse = ttPick.PrintHouse.
         
            LEAVE.
         END.   

         IF icPrintHouse > "" AND LOOKUP(lcPickPHouse,icPrintHouse) = 0 THEN 
            NEXT InvoiceSelect.
      END.

      liInvCnt = liInvCnt + 1.
      IF NOT SESSION:BATCH AND liInvCnt MOD 1000 = 0 THEN DO:
         PAUSE 0.
         DISP liInvCnt COLUMN-LABEL "Invoice Qty" 
            WITH 1 DOWN ROW 8 CENTERED TITLE " Collecting " 
            OVERLAY FRAME fQty.
      END.

      IF iiUpdInterval > 0 AND liInvCnt MOD iiUpdInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,liInvCnt) THEN
            RETURN "ERROR:Stopped".
      END.   
      
      CREATE ttInv.
      ASSIGN
         ttInv.InvNum     = Invoice.InvNum
         ttInv.PrintHouse = lcPickPHouse.

      IF Customer.IDelName > "" 
      THEN ttInv.ZipCode = Customer.IDelZipCode.
      ELSE ttInv.ZipCode = Customer.ZipCode.

      FOR EACH InvRow OF Invoice NO-LOCK WHERE
               InvRow.RowType = 2:
         ASSIGN
            ttInv.CallQty = ttInv.CallQty + InvRow.Qty
            liTotal       = liTotal + InvRow.Qty.
      END.

      IF ttInv.CallQty = 0 THEN ASSIGN
         ttInv.CallQty = 1
         liTotal = liTotal + 1.
                        
      FIND FIRST ttPrintHouse WHERE ttPrintHouse.PrintHouse = ttInv.PrintHouse
         NO-ERROR.
      IF AVAILABLE ttPrintHouse THEN DO:
         ttPrintHouse.InvQty = ttPrintHouse.InvQty + 1.
         IF ttPrintHouse.PortionPool THEN 
            liPortionTotal = liPortionTotal + 1.
      END.

   END.

   DELETE OBJECT lhField.
   DELETE OBJECT lhCustomer.

   /* check that given portions are not exceeded, if are then move to default
   */
   FIND FIRST bDefaultHouse WHERE 
      bDefaultHouse.PrintHouse = lcDefaultPHouse NO-ERROR.
      
   IF AVAILABLE bDefaultHouse THEN 
   FOR EACH ttPrintHouse WHERE
            ttPrintHouse.PortionPool AND
            ttPrintHouse.MaxPortion > 0:
            
      liPortionQty = ttPrintHouse.MaxPortion * liPortionTotal / 100.
      IF ttPrintHouse.InvQty > liPortionQty THEN       
      FOR EACH ttInv WHERE
               ttInv.PrintHouse = ttPrintHouse.PrintHouse:
         ASSIGN
            ttInv.PrintHouse = lcDefaultPHouse
            ttPrintHouse.InvQty = ttPrintHouse.InvQty - 1
            bDefaultHouse.InvQty = bDefaultHouse.InvQty + 1.

         IF ttPrintHouse.InvQty <= liPortionQty THEN LEAVE.
      END.
   END.

   RETURN "".
   
END PROCEDURE.
 
PROCEDURE pDivideIntoGroups: 

   DEF VAR liLimit       AS INT    NO-UNDO.
   DEF VAR liFeedOrder   AS INT  NO-UNDO.
   DEF VAR lcProcessHost AS CHAR NO-UNDO.
   DEF VAR liReplQty     AS INT  NO-UNDO.
   DEF VAR liReplLimit   AS INT  NO-UNDO.
   DEF VAR liMainLimit   AS INT  NO-UNDO.
   
   ASSIGN
      liMainLimit = liTotal / iiBatchQty
      liBatchCnt  = 0.

   fCalculateReplicaQty(iiUseReplica,
                        iiBatchQty,
                        liTotal,        
                        INPUT-OUTPUT liMainLimit,
                        OUTPUT liReplQty,
                        OUTPUT liReplLimit).
 
   /* invoices must be in zipcode order for doc1 in each printhouse file */
   FOR EACH ttPrintHouse:

      ASSIGN 
         liPHouseCnt = 0
         liCallQty   = -1
         liLimit = liMainLimit.

      FOR EACH ttInv WHERE
               ttInv.PrintHouse = ttPrintHouse.PrintHouse:

         /* because each printhouse is directed to it's own file, the total 
            number of files may exceed given batch qty */
         IF liCallQty < 0 OR liCallQty >= liLimit THEN DO:

            ASSIGN 
               liCallQty   = 0
               liBatchCnt  = liBatchCnt + 1
               liPHouseCnt = liPHouseCnt + 1
               liFeedOrder = 0.
    
            IF iiFRProcessID = 0 THEN DO:
               lcFile      = lcSplitDir + "/" + 
                             (IF icFileType BEGINS "XML" 
                              THEN "invxml_" 
                              ELSE "doc1_") +  
                             ttPrintHouse.PrintHouse + "_" + 
                             STRING(idaInvDate,"999999") + "_" + 
                             STRING(liPHouseCnt) + ".txt".
                 
               OUTPUT STREAM sFile CLOSE.
      
               OUTPUT STREAM sFile TO VALUE(lcFile).
         
               ocFileList = ocFileList + 
                           (IF ocFileList > "" THEN "," ELSE "") + lcFile.
            END.

            ELSE DO:
               ASSIGN 
                  lcProcessHost = ""
                  liLimit = liMainLimit.
               CASE iiUseReplica:
               /* partially to replica */
               WHEN 1 THEN DO:
                  IF liBatchCnt <= liReplQty THEN ASSIGN
                     lcProcessHost = lcReplica
                     liLimit = liReplLimit.
               END.
               /* all to replica */
               WHEN 2 THEN lcProcessHost = lcReplica.
               END CASE. 
            END.
         END.   

         ASSIGN    
            liCallQty = liCallQty + ttInv.CallQty
            oiInvCnt  = oiInvCnt + 1.
            
         IF iiFRProcessID > 0 THEN DO TRANS:
            CREATE FuncRunResult.
            ASSIGN 
               FuncRunResult.FRProcessID = iiFRProcessID
               FuncRunResult.FRExecID    = iiFRExecID
               FuncRunResult.FRResultSeq = liBatchCnt
               FuncRunResult.IntParam    = ttInv.InvNum
               liFeedOrder               = liFeedOrder + 1
               FuncRunResult.ResultOrder = liFeedOrder
               FuncRunResult.CharParam   = ttInv.PrintHouse
               FuncRunResult.ProcessHost = lcProcessHost.
         END.

         ELSE DO:
            PUT STREAM sFile UNFORMATTED 
               ttInv.InvNum "|" 
               ttInv.CallQty SKIP.
         END.
            
      END.
   
      /* mark last file of each printhouse */
      IF liPHouseCnt > 0 THEN DO:
         IF iiFRProcessID > 0 THEN DO:
            FOR EACH FuncRunResult WHERE
                     FuncRunResult.FRProcessID = iiFRProcessID AND
                     FuncRunResult.FRResultSeq = liBatchCnt:
               FuncRunResult.DecParam = 1.      
            END. 
         END.
         ELSE DO:
            PUT STREAM sFile UNFORMATTED
               "0|0|END" SKIP.
   
            OUTPUT STREAM sFile CLOSE.
         END.   
      END.
      
   END.

   RETURN "".

END PROCEDURE.   

PROCEDURE pMarkFinished:

   IF ilDBWrite THEN DO:
      /* mark this run finished */
      FOR FIRST bActionLog USE-INDEX ActionID WHERE
                bActionLog.Brand        = gcBrand     AND    
                bActionLog.ActionID     = lcActionID  AND
                bActionLog.ActionTS     = ldThisRun   AND
                bActionLog.ActionStatus = 0
      EXCLUSIVE-LOCK:
         ASSIGN 
            bActionLog.ActionStatus = 2
            bActionLog.ActionDec    = liInvCnt
            bActionLog.ActionChar   = STRING(liInvCnt) + " invoices picked".
      END.
   END.
   
   ELSE DO:
      PUT STREAM sAction UNFORMATTED
         icFileType "|"
         katun "|"
         lcActionID "|"
         idaInvDate "|"
         "Finished"  "|"
         STRING(TIME,"hh:mm:ss") "|"
         liInvCnt " invoices picked"
         SKIP.
   END.
   
END PROCEDURE.


