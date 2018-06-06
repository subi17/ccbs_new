/**
 * High spender

 * @input  brand;string;mandatory;tenant
 * @output custnum;int;mandatory
           personid;string;mandatory
           name;string
           address;string 
           cli;string
           subscription_type;string
           active_days;string
           reason;string
           user_age_group;string
           balance;double
           unbilled;double
           invoice_count;int
           average_amount;double
           period;string
           barring;string
           barring_created:datetime
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:katun = "NewtonAd".
Syst.Var:gcBrand = "1".
{Func/highusage_report.i}
{newton/src/json_key.i}
{Func/barrfunc.i}

DEF VAR lcArray AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR liPeriod AS INT NO-UNDO.
DEF VAR pcTenant AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcTenant  = get_string(param_toplevel_id, "0"). 

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

liPeriod = INT(STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99")).
lcArray = add_array(response_toplevel_id, "").

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
    DEF VAR lcCLIType AS CHAR NO-UNDO.
    
    IF piPeriod NE liPeriod THEN RETURN TRUE.  

    FIND HiUsageKat
    WHERE HiUsageKat.Category EQ phHighUsage.Category.
    
    FIND FIRST MobSub WHERE MobSub.CLI = phHighUsage.CLI NO-LOCK NO-ERROR.
    IF AVAIL MobSub THEN DO: 
      lcCLIType = MobSub.CliType.  
      lcActiveBarrings = Func.BarrMethod:mGetActiveBarrings(MobSub.MsSeq).
      
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
    add_string(lcStruct, "subscription_type",lcCLIType). 
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
   END.
