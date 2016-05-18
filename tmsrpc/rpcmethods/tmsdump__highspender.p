/**
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

DEF VAR lcArray AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR liPeriod AS INT NO-UNDO.

liPeriod = INT(STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99")).
lcArray = add_array(response_toplevel_id, "").

{Syst/commpaa.i}
katun = "NewtonAd".
gcBrand = "1".
{Func/highusage_report.i}
{Func/timestamp.i}
{rpcmethods/json_key.i}
{Func/barrfunc.i}

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
  
    DEF VAR ldeBarrActTS AS DEC NO-UNDO. 
    DEF VAR lcActiveBarrings AS CHAR NO-UNDO. 
    DEF VAR i AS INT NO-UNDO. 
    
    IF piPeriod NE liPeriod THEN RETURN TRUE.  

    FIND HiUsageKat
    WHERE HiUsageKat.Category EQ phHighUsage.Category.
    
    FIND FIRST MobSub WHERE MobSub.CLI = phHighUsage.CLI NO-LOCK NO-ERROR.
    IF AVAIL MobSub THEN DO: 
      
      lcActiveBarrings = fGetActiveBarrings(MobSub.MsSeq).
      
      IF lcActiveBarrings > "" THEN DO:
         DO i = 1 TO NUM-ENTRIES(lcActiveBarrings):
            FIND FIRST Barring NO-LOCK WHERE
                       Barring.MsSeq = MobSub.MsSeq AND
                       Barring.BarringCode = ENTRY(i,lcActiveBarrings)
            USE-INDEX MsSeq.
            IF AVAIL Barring AND
                     Barring.BarringStatus EQ {&BARR_STATUS_ACTIVE} AND
                     ldeBarrActTS < Barring.EventTS THEN
               ldeBarrActTS = Barring.EventTS.
         END.
      END.
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
    add_string(lcStruct, "barring", lcActiveBarrings).
    IF ldeBarrActTS > 0 THEN
       add_timestamp(lcStruct, "barring_created", ldeBarrActTS).

    RETURN TRUE.
END FUNCTION.

loop_highspender_table("1", 0.0, 0).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
