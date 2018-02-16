

DEFINE VARIABLE liDumpID AS INTEGER NO-UNDO.

FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile
    THEN liDumpID = DumpFile.DumpID + 1.
    ELSE liDumpID = 1.
    
   CREATE DumpFile.
   ASSIGN  
      DumpFile.Brand           = "1"
      DumpFile.DumpID          = liDumpID
      DumpFile.DumpName        = "TerminalFinancingReport"
      DumpFile.Description     = "Terminal Financing Report Dump"
      DumpFile.FileName        = "#TENANT_TERMINAL_FINANCING_#DATE.txt"
      DumpFile.SpoolDir        = "/tmp/spool/"
      DumpFile.TransDir        = "/tmp/transferDir"
      DumpFile.DumpDelimiter   = ","
      DumpFile.DecimalPoint    = ","
      DumpFile.LogicModule     = "Ar/terminal_financial_dump.p"
      DumpFile.MainTable       = "FixedFee"
      DumpFile.DumpFormat      = "ASCII"
      DumpFile.FileCategory    = "TRACK"
      DumpFile.EmptyFile       = YES 
      DumpFile.ModFromEventLog = TRUE
      DumpFile.AllowReplica    = NO.
      DumpFile.Active          = TRUE .
      
FUNCTION fCreateDFField RETURNS LOGICAL 
    ( INPUT icLabel AS CHARACTER , INPUT icField AS CHARACTER , INPUT iiOrder AS INTEGER) :
  CREATE DFField.
  ASSIGN 
      DFField.DumpID        =  DumpFile.DumpID
      DFField.Brand         = "1"
      DFField.DFLabel       = icLabel
      DFField.DFField       = icField
      DFField.OrderNbr      = iiOrder 
      DFField.FromDate      = TODAY 
      DFField.ToDate        = 12/31/49.   
END FUNCTION.    

fCreateDFField ( "Bank Code"      , "#BANKCODE"    , 1 ) .
fCreateDFField ( "Bank Code Name" , "#BANKNAME"    , 2 ) .
fCreateDFField ( "Run Date"       , "#RUNDATE"     , 3 ) .
fCreateDFField ( "PayTerm Qty"    , "#PAYTERMQTY"  , 4 ) .
fCreateDFField ( "Q25 Qty"        , "#Q25QTY"      , 5 ) .
fCreateDFField ( "Total Amount"   , "#TOTALAMT"    , 6 ) .
fCreateDFField ( "PayTerm Amount" , "#PAYTERMAMT"  , 7 ) .
fCreateDFField ( "Q25 Amount"     , "#Q25AMT"      , 8 ) .
fCreateDFField ( "Q25 Extension Amount"    , "#Q25EXTNAMT"  , 9 ) .

         
      