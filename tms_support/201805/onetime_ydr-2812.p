
/*------------------------------------------------------------------------
    File        : onetime_ydr-2812.p
    Purpose     : Onetime fix to create dumpfile record for mnp sms rejection report

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue May 15 12:09:38 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE liDumpID    AS INTEGER NO-UNDO.

FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile
    THEN liDumpID = DumpFile.DumpID + 1.
    ELSE liDumpID = 1.
    
   CREATE DumpFile.
   ASSIGN  
      DumpFile.Brand           = "1"
      DumpFile.DumpID          = liDumpID
      DumpFile.DumpName        = "SmsRejectedPortability"
      DumpFile.Description     = "Sms Rejected Portability Report Dump"
      DumpFile.FileName        = "#TENANT_Sms_Rejected_Portability_#DATE.dump"
      DumpFile.SpoolDir        = "/store/riftp/dumpfiles/dwh/spool/"
      DumpFile.TransDir        = "/store/riftp/dumpfiles/dwh/outgoing/"
      DumpFile.DumpDelimiter   = "|"
      DumpFile.DecimalPoint    = "."
      DumpFile.LogicModule     = "Mnp/mnpsmsreject.p"
      DumpFile.MainTable       = "mnpprocess"
      DumpFile.DumpFormat      = "ASCII"
      DumpFile.FileCategory    = "SmsRejectedPortability"
      DumpFile.EmptyFile       = YES 
      DumpFile.ModFromEventLog = TRUE
      DumpFile.AllowReplica    = NO.
      DumpFile.Active          = TRUE .
