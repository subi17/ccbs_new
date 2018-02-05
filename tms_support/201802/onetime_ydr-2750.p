/*
  Onetime script to fix existing 
      last FFItem's based in Base Bundles
*/


  DEFINE VARIABLE liLastActiveDays AS INTEGER NO-UNDO.
  DEFINE VARIABLE liLastActiveAmt  AS DECIMAL NO-UNDO.
  DEFINE VARIABLE liLastMonthDays  AS INTEGER NO-UNDO.
  DEFINE VARIABLE lisMainBundleMF  AS LOGICAL NO-UNDO.
  
FUNCTION fCheckBundleFMItem RETURNS LOGICAL 
    ( INPUT icBrand     AS CHARACTER  ,
      INPUT icFeeModel  AS CHARACTER  ,
      INPUT icBillCode  AS CHARACTER  ,
      INPUT icBundle    AS CHARACTER ) :
    DEFINE BUFFER DayCampaign FOR DayCampaign.
    DEFINE BUFFER Feemodel    FOR FeeModel.
    DEFINE BUFFER FMItem      FOR FMItem.
    
    FOR FIRST DayCampaign NO-LOCK WHERE
              DayCampaign.Brand   = icBrand  AND
              DayCampaign.DCEvent = icBundle ,
        FIRST FeeModel NO-LOCK WHERE
              FeeModel.Brand    = icBrand AND
              FeeModel.FeeModel = DayCampaign.FeeModel,
        FIRST FMItem NO-LOCK WHERE
              FMItem.Brand    = icBrand    AND
              FMItem.FeeModel = icFeeModel AND 
              FMItem.BillCode = icBillCode :
        RETURN TRUE.                 
    END.    
    RETURN FALSE.
END FUNCTION. 

FOR EACH MobSub NO-LOCK WHERE MobSub.MsStatus = 4 :
    FIND cliType WHERE clitype.clitype = MobSub.CliType NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CLIType THEN NEXT.
    FOR EACH Fixedfee WHERE 
             Fixedfee.brand     = MobSub.Brand AND 
             Fixedfee.hosttable = "mobsub"     AND 
             Fixedfee.keyvalue  = STRING(MobSub.MsSeq) NO-LOCK:
       lisMainBundleMF = fCheckBundleFMItem (FixedFee.Brand , FixedFee.FeeModel ,FixedFee.BillCode , CLIType.Clitype ) .
       IF NOT lisMainBundleMF AND CLIType.BaseBundle NE "" THEN 
            lisMainBundleMF = fCheckBundleFMItem (FixedFee.Brand , FixedFee.FeeModel ,FixedFee.BillCode , CLIType.BaseBundle ) .
       IF NOT lisMainBundleMF AND CLIType.FixedBundle NE "" THEN 
            lisMainBundleMF = fCheckBundleFMItem (FixedFee.Brand , FixedFee.FeeModel ,FixedFee.BillCode , CLIType.FixedBundle) .
       IF DAY(FixedFee.BegDate) NE 1 AND lisMainBundleMF THEN DO TRANS: 
           FIND LAST FFItem OF FixedFee EXCLUSIVE-LOCK NO-ERROR.
           IF NOT AVAILABLE FFItem OR ffitem.billed OR FFItem.Concerns[1] < 999999  THEN NEXT.
           ASSIGN
                liLastActiveDays   = DAY(FixedFee.BegDate) - 1 
                liLastMonthDays    = FFItem.Concerns[2] MOD 100
                FFItem.Concerns[2] = (( INTEGER( FFItem.Concerns[2] / 100 ) ) * 100 ) + liLastActiveDays
                liLastActiveAmt    = ( FixedFee.Amt / liLastMonthDays ) * liLastActiveDays 
                FFItem.Amt         = liLastActiveAmt.
       END.                 
    END.    
END.





