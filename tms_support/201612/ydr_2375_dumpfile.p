
DEF VAR i AS INT NO-UNDO.

FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN
   i = DumpFile.DumpID + 1.
ELSE
   i = 1.

CREATE DumpFile.
ASSIGN DumpFile.Brand           = '1'
       DumpFile.Description     = 'Credit Limit Dump'
       DumpFile.DumpID          = i
       DumpFile.FileName        = '#CAT_CreditLimit_#MODE_#DATE_#TIME.txt'
       DumpFile.SpoolDir        = '/store/riftp/dumpfiles/dwh/spool'
       DumpFile.TransDir        = '/store/riftp/dumpfiles/dwh/outgoing'
       DumpFile.Active          = YES
       DumpFile.DumpDelimiter   = '|'
       DumpFile.DecimalPoint    = '.'
       DumpFile.MainTable       = 'Limit'
       DumpFile.DumpName        = 'CreditLimit'
       DumpFile.DumpFormat      = 'ASCII'
       DumpFile.EmptyFile       = YES
       DumpFile.FileCategory    = 'DWH'
       DumpFile.LogicModule     = 'creditlimitdump'
       DumpFile.BatchID         = 1
       .

CREATE DFTimeTable.
ASSIGN DFTimeTable.Brand       = "1"
       DFTimeTable.DumpID      = i
       DFTimeTable.DumpMode    = "Full"
       DFTimeTable.FromDate    = TODAY + 1
       DFTimeTable.ToDate      = TODAY + 1
       DFTimeTable.DumpDay     = "*"
       DFTimeTable.DumpTime    = "1:45"
       .

CREATE DFTimeTable.
ASSIGN DFTimeTable.Brand       = "1"
       DFTimeTable.DumpID      = i
       DFTimeTable.DumpMode    = "Modified"
       DFTimeTable.FromDate    = TODAY + 2
       DFTimeTable.ToDate      = 01/01/2050 - 1
       DFTimeTable.DumpDay     = "*"
       DFTimeTable.DumpTime    = "1:45"
       .

