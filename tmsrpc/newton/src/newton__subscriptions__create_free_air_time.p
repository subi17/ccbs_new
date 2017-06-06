/**
 * create FATime for an specific 
 * for an specific mobssub
 * 
 *
 * @input msseq;int;mandatory;id of subscription
 *        username;string;mandatory; who create the FATime
          fat;structure;mandatory; fatime details
 * @fat   fat_group;string;mandatory; FATGroup id
          period;string;mandatory; mm-yyyy 
          sms_amount;int;optional; FATime amt in case of sms 
          user_sms_limit;int;optional; sms monthly limit
          euro_amount;decimal;optional; FATime amt in case of euros 
          user_euro_limit;decimal;optional; euro monthly limit 
  * @output
*/
 {fcgi_agent/xmlrpc/xmlrpc_access.i}
 /* Input parameters */
 DEFINE VARIABLE piMsSeq AS INTEGER NO-UNDO. 
 DEFINE VARIABLE pcUserName AS CHARACTER NO-UNDO. 
 DEFINE VARIABLE pcStruct AS CHARACTER NO-UNDO. 
 DEFINE VARIABLE lcStruct AS CHARACTER NO-UNDO. 
 DEFINE VARIABLE lcFatGroup AS CHARACTER NO-UNDO. 
 DEFINE VARIABLE lcFromPeriod AS CHARACTER NO-UNDO. 
 /* Output parameters */
 /* Local variables */
 DEFINE VARIABLE lcBrand       AS CHARACTER NO-UNDO INIT "1".
 DEFINE VARIABLE ldtChkDate    AS DATE NO-UNDO.  
 DEFINE VARIABLE liFromPeriod  AS INT NO-UNDO. 
 DEFINE VARIABLE ldAmt         AS DEC NO-UNDO.
 DEFINE VARIABLE ldMonthAmt    AS DEC NO-UNDO. 
 DEFINE VARIABLE ldUserLimit   AS DEC NO-UNDO.
 DEFINE VARIABLE ldTS1         AS DEC NO-UNDO.
 DEFINE VARIABLE ldTS2         AS DEC NO-UNDO.
 DEFINE VARIABLE ldTS3         AS DEC NO-UNDO.
 DEFINE VARIABLE liCounterType AS INTEGER NO-UNDO. 
 DEFINE VARIABLE lcError       AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "int,string,struct") EQ ? THEN RETURN.

piMsSeq = get_int(param_toplevel_id, "0").
pcUserName = "VISTA_" + get_string(param_toplevel_id, "1").
pcStruct = get_struct(param_toplevel_id,"2").

IF gi_xmlrpc_error NE 0 THEN RETURN.
lcStruct = validate_struct(pcStruct, "fat_group!,period!," + 
                                     "sms_amount,user_sms_limit," + 
                                     "euro_amount,user_euro_limit").


lcFatGroup = get_string(pcStruct,"fat_group").
lcFromPeriod = get_string(pcStruct,"period").
liFromPeriod = INT( ENTRY(2,lcFromPeriod,"-") + ENTRY(1,lcFromPeriod,"-") ) .

/* pick up amount */
IF LOOKUP("sms_amount", lcStruct) GT 0 AND 
   LOOKUP("user_sms_limit",lcStruct) GT 0 THEN DO:
   ldAmt = DEC(get_int(pcStruct,"sms_amount")).
   ldUserLimit = DEC(get_int(pcStruct,"user_sms_limit")).
   liCounterType = 2.
  
END.
ELSE IF LOOKUP("euro_amount", lcStruct) GT 0 AND 
        LOOKUP("user_euro_limit",lcStruct) GT 0 THEN DO:
   ldAmt = get_double(pcStruct,"euro_amount").
   ldUserLimit = get_double(pcStruct,"user_euro_limit").
   liCounterType = 3.
END.
ELSE RETURN appl_err("Invalid amount and limit").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

{Syst/commpaa.i}
katun = pcUserName.
gcbrand = "1".
{Func/fixedfee.i}
{Func/fcounter.i}
/* Check that mobsub is available */
FIND MobSub WHERE
     MobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN DO:
   RETURN appl_err("Mobile Subscription not available").
END.
 /* check FATGroup */
 FIND FatGroup WHERE
      FatGroup.Brand = gcBrand AND
      FatGroup.FtGrp = lcFatGroup  NO-LOCK NO-ERROR.
 IF NOT AVAIL FatGroup THEN DO:
    RETURN appl_err("Unknown FAT Group").
 END.
 /* check period */
 ldtChkDate = fInt2Date(liFromPeriod,1).
 IF ldtChkDate = ? THEN DO:
    RETURN appl_err("Invalid period").
END.
 /* check monthly limits */
 fMonthlyStamps(TODAY,
                ldTS1,
                ldTS2). 
 FOR EACH Counter NO-LOCK WHERE 
          Counter.Brand = gcBrand AND
          Counter.HostTable = "MobSub" AND
          Counter.KeyValue = STRING(MobSub.MsSeq) AND
          Counter.CounterType = liCounterType AND
          Counter.EndStamp <= ldTS2 AND
          Counter.BeginStamp >= ldTS1 : 
        ldMonthAmt = ldMonthAmt + Counter.CounterAmt.
 END.
 IF ( ldMonthAmt + ldAmt) > ldUserLimit THEN
        RETURN appl_err("Change exceeds the monthly limit ").

 /* create FAtime */
 RUN Mc/creafat.p (MobSub.CustNum,
              MobSub.MsSeq,
              lcFatGroup,
              "",
              "",
              ldAmt,
              0,
              ?,
              liFromPeriod,
              999999,
              OUTPUT lcError).
 IF lcError > "" THEN
     RETURN appl_err("FATime event could not be created;" + lcError).

 /* update/create the counter */
 fUpdateCounter("MobSub",
                STRING(MobSub.MsSeq),
                liCounterType,
                ldTS2,
                ldTS1,
                ldAmt).

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
