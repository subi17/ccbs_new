/**
 */
{xmlrpc/xmlrpc_access.i}

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

DEF VAR lcArray AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR lcBarrComList AS CHAR NO-UNDO.
DEF VAR lcBarrStatus AS CHAR NO-UNDO.
DEF VAR ldaBarrCreated AS DATE NO-UNDO.
DEF VAR liSec AS INT NO-UNDO.
DEF VAR liPeriod AS INT NO-UNDO.

liPeriod = INT(STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99")).
lcArray = add_array(response_toplevel_id, "").

{commpaa.i}
katun = "NewtonAd".
gcBrand = "1".
{highusage_report.i}
{timestamp.i}
{json_key.i}

FUNCTION process_highspender_row RETURN LOGICAL
      ( BUFFER phInvCust FOR Customer,
        BUFFER phHighUsage FOR HighUsage,
        pcActiveDays AS CHAR,
        piCategoryLimit AS INT,
        pdTotalUnbilled AS DECIMAL,
        piInvoiceCount AS INT,
        pdInvoiceAverage AS DECIMAL,
        llHasOpenInv AS LOGICAL,
        llHasClaim AS LOGICAL,
        pcStatusName AS CHAR,
        BUFFER phMemo FOR Memo,
        piPeriod AS INT
      ):
  
    IF piPeriod NE liPeriod THEN RETURN TRUE.  

    ASSIGN
      lcBarrStatus = ""
      ldaBarrCreated = ?.

    FIND HiUsageKat
    WHERE HiUsageKat.Category EQ phHighUsage.Category.
    
    FIND FIRST MobSub WHERE MobSub.CLI = phHighUsage.CLI NO-LOCK NO-ERROR.
    IF AVAIL MobSub THEN DO: 
      
      RUN checkmsbarring(
         MobSub.MsSeq,
         katun,
         OUTPUT lcBarrComList,
         OUTPUT lcBarrStatus).
      
      IF LOOKUP(lcBarrStatus,"NONE,ONC,NAD") = 0 THEN DO:   
         FOR EACH MsRequest NO-LOCK WHERE
                  MsRequest.MsSeq = MobSub.MsSeq AND
                  MsRequest.ReqType = 35    AND
                  MsRequest.ReqStat = 2
         BREAK BY MsRequest.ReqStat BY MsRequest.DoneStamp: 
         /* Get last barring IF last was removal then no barring on */
            IF LAST-OF(MsRequest.ReqStat) THEN DO:
               fSplitTS(MsRequest.DoneStamp,OUTPUT ldaBarrCreated,OUTPUT liSec).
            END.
         END.  
       END.
       ELSE IF lcBarrStatus = "NONE" THEN lcBarrStatus = "".
    END.

    lcStruct = add_json_key_struct(lcArray, "").
    add_int(lcStruct, "custnum", phInvCust.CustNum).
    add_string(lcStruct, "personid", phInvCust.OrgId).
    add_string(lcStruct, "name", phInvCust.FirstName + " " +
                                 phInvCust.CustName + " " +
                                 phInvCust.SurName2).
    add_string(lcStruct, "address", phInvCust.Address + "\n" +
                                phInvCust.ZipCode + " " +
                                phInvCust.PostOffice).
    add_string(lcStruct, "cli", phHighUsage.CLI).
    add_string(lcStruct, "active_days", pcActiveDays).
    add_string(lcStruct, "reason", phHighUsage.Category).
    add_string(lcStruct, "user_age_group", SUBST("&1 - &2",
                        HiUsageKat.AgeFrom, HiUsageKat.AgeTo)).
    add_double(lcStruct, "balance", HighUsage.Amount).
    add_double(lcStruct, "unbilled", pdTotalUnbilled).
    add_int(lcStruct, "invoice_count", piInvoiceCount).
    add_double(lcStruct, "average_amount", pdInvoiceAverage).
    add_string(lcStruct, "period", SUBST("&1/&2", piPeriod MOD 100,
                                                  INT(piPeriod / 100))).
    add_string(lcStruct, "barring", lcBarrStatus).
    IF lcBarrStatus NE "" AND lcBarrStatus NE "ONC" THEN 
       add_date_or_time(lcStruct, "barring_created", ldaBarrCreated, liSec).

    RETURN TRUE.
END FUNCTION.

loop_highspender_table("1", 0.0, 0).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
