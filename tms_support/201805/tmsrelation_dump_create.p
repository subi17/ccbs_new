
/*------------------------------------------------------------------------
    File        : tmsrelation_dump_create.p
    Purpose     : 

    Syntax      :

    Description : It will create the Dump Records for the TMS Relation		

    Author(s)   : Koundinya Maddali
    Created     : Wed Jun 06 16:10:07 IST 2018
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
       DumpFile.DumpName        =     "HPD_TMSRelation"
       DumpFile.Description     =     "TMSRelation dump to HPD"
       DumpFile.FileName        =     "#TENANT_tmsrelation_#DATE_#TIME.txt"
       DumpFile.spooldir        =     "/store/riftp/pupu_dumps/spool"
       DumpFile.TransDir        =     "/mnt/store/riftp/hpd_st_dumps/outgoing"
       DumpFile.DumpDelimiter   =     "|"
       DumpFile.DecimalPoint    =     "."
       DumpFile.LogicModule     =     "HPD/hpd_filedump.p"
       DumpFile.LinkKey         =     "HPD.GenericDump"
       DumpFile.DumpCharSet     =     "UTF-8"
       DumpFile.DumpFormat      =     "ASCII"
       DumpFile.FileCategory    =     "HPD" 
       DumpFile.AveDurFull      =     1      
       DumpFile.MainTable       =     "TMSRelation"
       DumpFile.SideTables      =     "Common"
       DumpFile.BatchID         =     1
       DumpFile.Active          =     YES
       .
       
fCreateDumpFileds(INPUT "TMSRelationID", INPUT "TMSRelationID" ,   INPUT 1 ).
fCreateDumpFileds(INPUT "@version",      INPUT "1"             ,   INPUT 2 ).
fCreateDumpFileds(INPUT "TMSRelationID", INPUT "TMSRelationID" ,   INPUT 3 ).
fCreateDumpFileds(INPUT "TableName"    , INPUT "TableName"     ,   INPUT 4 ).
fCreateDumpFileds(INPUT "KeyType"      , INPUT "KeyType"       ,   INPUT 5 ).
fCreateDumpFileds(INPUT "ParentValue"  , INPUT "ParentValue"   ,   INPUT 6 ).
fCreateDumpFileds(INPUT "ChildValue"   , INPUT "ChildValue"    ,   INPUT 7 ).
fCreateDumpFileds(INPUT "RelationType" , INPUT "RelationType"  ,   INPUT 8 ).
fCreateDumpFileds(INPUT "FromTime"     , INPUT "FromTime"      ,   INPUT 9 ).
fCreateDumpFileds(INPUT "ToTime"       , INPUT "ToTime"        ,   INPUT 10 ).


CREATE DumpHPD.
ASSIGN DumpHPD.DumpID     =  DumpFile.DumpID
       DumpHPD.Active     =  DumpFile.Active
       DumpHPD.Continuous =  YES
       .
          
