{Syst/commpaa.i}
gcbrand = "1".
katun = "Qvantel".
{Func/ftransdir.i}
{Func/timestamp.i}
{Inv/printdoc1tt.i}

def stream sInputFile.
def stream sout.
def stream sFile.

DEFINE TEMP-TABLE ttBatchInputFile
   FIELD FileName  AS CHAR
   FIELD Directory AS CHAR
   INDEX FileName IS PRIMARY UNIQUE FileName.

DEFINE TEMP-TABLE ttInputFileContent
   FIELD FileName   AS CHAR
   FIELD LineNo     AS INT
   FIELD InputLine  AS CHAR
   INDEX FileNameNo FileName LineNo.

def var lcline        as char no-undo.
def var liInvCount    as int no-undo.
def var ldaInvDate    as date no-undo.
def var lcIncomingDir as char no-undo.
def var lcOutputDir   as char no-undo.
def var liPrinted     as int no-undo.
def var liLine        as int no-undo.
def var ldThisRun     as dec no-undo.
def var lcLockFile    as char no-undo.
def var liFiles       as int no-undo.
def var liTotalInvoices as int no-undo.
def var lcDir         as char no-undo.
def var lcResultFile  as char no-undo.
def var lcLogFile     as char no-undo.

FORM
   SKIP(1)
   " Total Files Handled..: " liFiles   FORMAT ">>>>>>>>>9" SKIP
   " Total Invoice Handled: " liTotalInvoices FORMAT ">>>>>>>>>9" SKIP
   SKIP(1)
WITH
   CENTERED NO-LABELS TITLE " XML Creation Tool " WIDTH 50 ROW 8
FRAME frmMain.

IF lcLockFile = ? OR lcLockFile = "" THEN
   lcLockFile = "/tmp/xml_file_creation_daemon.lock".

UNIX SILENT VALUE("touch " + lcLockFile).

ASSIGN lcIncomingDir = "/apps/yoigo/tms_support/billing/log/invoices/".
       lcOutputDir = lcIncomingDir + "output/".

MAIN_LOOP:
DO WHILE TRUE
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

    IF RETRY THEN LEAVE.

    /* should I be running */
    FILE-INFO:FILE-NAME = lcLockFile.
    IF FILE-INFO:FILE-TYPE = ? THEN LEAVE.

    /* Empty Temp-table */
    EMPTY TEMP-TABLE ttBatchInputFile.
    EMPTY TEMP-TABLE ttInputFileContent.
    EMPTY TEMP-TABLE ttInvoice.

    ASSIGN ldThisRun = fMakeTS()
           liPrinted = 0
           liTotalInvoices = liTotalInvoices + liInvCount
           liInvCount = 0.

    INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcIncomingDir + "*.log | head -1").

    FILE_LOOP:
    REPEAT:

      IMPORT STREAM sFile UNFORMATTED lcResultFile.

      IF SEARCH(lcResultFile) NE ? THEN DO:
         ASSIGN liLine = 0
                liFiles = liFiles + 1.

         CREATE ttBatchInputFile.
         ASSIGN
            ttBatchInputFile.FileName = REPLACE(ENTRY(NUM-ENTRIES(lcResultFile,"/"),
                                        lcResultFile,"/"),".log","")
            ttBatchInputFile.Directory = REPLACE(ttBatchInputFile.FileName,"invoices","")
            ttBatchInputFile.Directory = REPLACE(ttBatchInputFile.Directory,"_","").

         INPUT STREAM sInputFile FROM VALUE(lcResultFile).

         REPEAT:
            IMPORT STREAM sInputFile UNFORMATTED lcLine.
            liLine = liLine + 1.
            IF lcLine = "" THEN NEXT.

            CREATE ttInputFileContent.
            ASSIGN
               ttInputFileContent.LineNo    = liLine
               ttInputFileContent.InputLine = lcLine
               ttInputFileContent.FileName  = ttBatchInputFile.FileName.
         END. /* REPEAT: */
         INPUT STREAM sInputFile CLOSE.
      END. /* IF SEARCH(lcResultFile) NE ? THEN DO: */

      /* Moved input file to processed directory */
      fTransDir(lcResultFile,
                ".log",
                lcOutputDir).
    END. /* REPEAT: */

    INPUT STREAM sFile CLOSE.

    DISP
       liFiles
       liTotalInvoices WITH FRAME frmMain.
    PAUSE 0.

    FILE_LOOP:
    FOR FIRST ttBatchInputFile:

       assign lcLogFile = lcOutputDir + ttBatchInputFile.FileName +
                          "_done_" + STRING(ldThisRun) + ".log"
              lcDir = ttBatchInputFile.Directory.
       OUTPUT STREAM sout TO VALUE(lcLogFile).

       FILE_CONTENT_LOOP:
       FOR EACH ttInputFileContent WHERE
                ttInputFileContent.FileName = ttBatchInputFile.FileName:

          lcLine = ttInputFileContent.InputLine.

          IF liInvCount = 0 THEN DO:
             ldaInvDate = DATE(entry(3,lcline,"|")) no-error.
             if ldaInvDate = ? then do:
                put stream sout unformatted lcline "|ERROR:Invoice date is blank" skip.
                LEAVE FILE_CONTENT_LOOP.
             end.
          end.

          find first invoice where
                     invoice.brand = "1" and
                     invoice.extinvid = entry(1,lcline,"|") no-lock no-error.
          if not avail invoice then do:
             put stream sout unformatted lcline "|ERROR:Invoice not found" skip.
             next.
          end.

          if invoice.invtype <> 1 then do:
             put stream sout unformatted lcline "|ERROR:Invalid Invoice Type: " STRING(invoice.invtype) skip.
             next.
          end.

          IF Invoice.InvCfg[1] = TRUE THEN DO:
             put stream sout unformatted lcline "|ERROR:Printing Denied" skip.
             next.
          end.

          find first customer where
                     customer.custnum = invoice.custnum no-lock no-error.
          if not avail customer then do:
             put stream sout unformatted lcline "|ERROR:Customer not found" skip.
             next.
          end.

          if can-find(first ttInvoice where ttInvoice.invnum = invoice.invnum) then do:
             put stream sout unformatted lcline "|ERROR:Duplicate Invoice" skip.
             next.
          end.

          CREATE ttInvoice.
          ASSIGN ttInvoice.InvNum = invoice.invnum
                 liInvCount = liInvCount + 1.

          IF Customer.IDelName > "" 
          THEN ttInvoice.ZipCode = Customer.IDelZipCode.
          ELSE ttInvoice.ZipCode = Customer.ZipCode.

          status default invoice.extinvid + "-" + string(liInvCount).
          put stream sout unformatted lcline "|Done" skip.
       end.
    end.

    find first ttInvoice no-lock no-error.
    if avail ttInvoice then do:
       RUN /apps/yoigo/tms/Inv/invoice_xml.p (INPUT-OUTPUT TABLE ttInvoice,
                          ldaInvDate,
                          liInvCount,
                          TRUE,
                          "/mnt/xmlstore/old_xmls_201510" + lcDir + "*/store/print/xmlspool/#INVNUM.xml",
                          "QVANTEL",
                          0,
                          FALSE,
                          0,
                          0,
                          OUTPUT liPrinted).
       put stream sout unformatted "Following invoices handled: " string(liPrinted) skip.
   end.
   output stream sout close.
end.
   
