
/*------------------------------------------------------------------------
    File        : customeraccount_dump_create.p
    Purpose     : 

    Syntax      :

    Description : It will create the Dump Records for the CustomerAccount      

    Author(s)   : srvuddan
    Created     : 08-08-2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


DEFINE VARIABLE liDumpID AS INTEGER NO-UNDO.

FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile
    THEN liDumpID = DumpFile.DumpID + 1.
ELSE liDumpID = 1.
    
    
FUNCTION fCreateDumpFileds RETURNS CHARACTER (INPUT icDFField AS CHARACTER ,
                                              INPUT icDFLabel AS CHARACTER ,
                                              INPUT iiOrder   AS INTEGER) :
                                
    CREATE DFField.
    ASSIGN DFField.Brand      =     DumpFile.Brand
           DFField.DFField    =     icDFField
           DFField.DFLabel    =     icDFLabel
           DFField.DFTable    =     DumpFile.MainTable
           DFField.DumpID     =     DumpFile.DumpID
           DFField.FromDate   =     TODAY
           DFField.ToDate     =     DATE(12,31,2049)
           DFField.OrderNbr   =     iiOrder. 
           
     RETURN "".
                                
END FUNCTION.                                    
    
CREATE DumpFile.
ASSIGN DumpFile.Brand           =     Syst.Var:gcBrand
       Dumpfile.DumpID          =     liDumpID
       DumpFile.DumpName        =     "HPD_CustomerAccount"
       DumpFile.Description     =     "CustomerAccount dump to HPD"
       DumpFile.FileName        =     "#TENANT_customeraccount_#DATE_#TIME.txt"
       DumpFile.spooldir        =     "/store/riftp/pupu_dumps/spool"
       DumpFile.TransDir        =     "/mnt/store/riftp/hpd_st_dumps/outgoing"
       DumpFile.DumpDelimiter   =     "|"
       DumpFile.DecimalPoint    =     "."
       DumpFile.LogicModule     =     "HPD/hpd_filedump.p"
       DumpFile.LinkKey         =     "HPD.CustomerAccountDump"
       DumpFile.DumpCharSet     =     "UTF-8"
       DumpFile.DumpFormat      =     "ASCII"
       DumpFile.FileCategory    =     "HPD" 
       DumpFile.AveDurFull      =     1      
       DumpFile.MainTable       =     "CustomerAccount"
       DumpFile.SideTables      =     "Common"
       DumpFile.BatchID         =     1
       DumpFile.Active          =     YES
       .
       
fCreateDumpFileds(INPUT "AccountID"    , INPUT "AccountID"   ,   INPUT 1 ).
fCreateDumpFileds(INPUT "@version"     , INPUT "1"           ,   INPUT 2 ).
fCreateDumpFileds(INPUT "AccountID"    , INPUT "AccountID"   ,   INPUT 3 ).
fCreateDumpFileds(INPUT "CustNum"      , INPUT "CustNum"     ,   INPUT 4 ).
fCreateDumpFileds(INPUT "DefaultAcc"   , INPUT "DefaultAcc"  ,   INPUT 5 ).
fCreateDumpFileds(INPUT "AccountName"  , INPUT "AccountName" ,   INPUT 6 ).
fCreateDumpFileds(INPUT "FromDate"     , INPUT "FromDate"    ,   INPUT 7 ).
fCreateDumpFileds(INPUT "ToDate"       , INPUT "ToDate"      ,   INPUT 8 ).
fCreateDumpFileds(INPUT "StatusCode"   , INPUT "StatusCode"  ,   INPUT 9 ).


CREATE DumpHPD.
ASSIGN DumpHPD.DumpID     =  DumpFile.DumpID
       DumpHPD.Active     =  DumpFile.Active
       DumpHPD.Continuous =  YES
       .
          