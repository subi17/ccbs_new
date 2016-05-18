/**
 * Usage counter 
 *
 * @input msseq;int
 * @output counters;array
   @counters tmcounters;struct
   @tmcounters id;int;number of counter
               name;string;tmrule name
               usage;double;tmrule amount
               unit_type;string;type of value
               limit_;double;limit_x
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.
/* Output parameters */
DEF VAR resp_struct AS CHAR NO-UNDO.
/* Local variables */
DEF VAR liCount AS INT NO-UNDO.
DEF VAR tmcounters AS CHARACTER NO-UNDO. 
DEF VAR tmcounter_struct AS CHARACTER NO-UNDO. 
DEF VAR lcValueType AS CHARACTER NO-UNDO. 
DEF VAR lcValue AS CHARACTER NO-UNDO. 
DEF VAR liLimitSource AS INT NO-UNDO. 
DEF VAR llDSSActive   AS LOG NO-UNDO.
DEF VAR ldeLimitAmt   AS DEC NO-UNDO.
DEF VAR lcDSSBundleId AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND mobsub NO-LOCK WHERE
     mobsub.msseq = piMsSeq NO-ERROR.
IF NOT AVAILABLE mobsub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

{Syst/commpaa.i}
katun = "Newton".
gcBrand = "1".
{Func/dataformat.i}
{Func/fdss.i}

resp_struct = add_struct(response_toplevel_id, "").

/* add TMCounters of the mobsub */
tmcounters = add_array(resp_struct,"counters").

/* Fee calculation changes */
IF NOT MobSub.PayType THEN
   lcDSSBundleId = fGetActiveDSSId(INPUT Mobsub.Custnum,
                                   INPUT fMakeTS()).

IF lcDSSBundleId = "DSS2" THEN
   lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

IF lcDSSBundleId = {&DSS} OR
   (lcDSSBundleId = "DSS2" AND
    LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN
   llDSSActive = TRUE.

liCount = 0.

FOR EACH TMRule  NO-LOCK WHERE
         TMRule.Brand = gcBrand AND
         TMRule.FromDate <= TODAY AND
         TMRule.ToDate >=  TODAY:

    FOR EACH TMCounter NO-LOCK WHERE 
             TMCounter.CustNum = MobSub.CustNum AND
             TMCounter.TMRuleSeq = TMRule.TMRuleSeq AND
             TMCounter.FromDate <= TODAY AND
             TMCounter.ToDate >= TODAY :

         /* YTS-3802 */
         IF TMCounter.Amount = 0 THEN NEXT.

         /* Don't display DSS usage on prepaid subs. */
         IF STRING(TMCounter.TMRuleSeq) = fCParamC("TMQueueDSSUpsell") THEN DO:
            IF NOT llDSSActive THEN NEXT.
         END. /* IF STRING(TMCounter.TMRuleSeq) */
         ELSE IF TMCounter.MsSeq <> MobSub.MsSeq THEN NEXT.

         tmcounter_struct = add_struct(tmcounters,"").
         liCount = liCount + 1.

         add_int(tmcounter_struct,"id",liCount).
         add_string(tmcounter_struct,"name",TMRule.Name).
         add_double(tmcounter_struct,"usage",TMCounter.Amount).

         RUN fFormatUnit(
                     TMRule.CounterAmount,
                     TMCounter.Amount,
                     OUTPUT lcValueType,
                     OUTPUT lcValue). 
         add_string(tmcounter_struct,"unit_type",lcValueType).
   
         /* use customer limit if it is found, otherwise use default limit */
         IF TMRule.LimitSource = 4 THEN DO:
            IF CAN-FIND(FIRST Limit NO-LOCK USE-INDEX CustNum WHERE
                              Limit.CustNum   = TMCounter.CustNum AND
                              Limit.LimitType = {&LIMIT_TYPE_TMRLIMIT} AND
                              Limit.TMRuleSeq = TMCounter.TMRuleSeq AND
                              Limit.ToDate   >= TODAY AND
                              Limit.FromDate <= TODAY)
            THEN liLimitSource = 1.
            ELSE liLimitSource = 3.
         END.
         ELSE liLimitSource = TMRule.LimitSource.
         
         IF liLimitSource NE 3 THEN DO:
            FOR EACH Limit NO-LOCK WHERE
                     Limit.CustNum = TMCounter.CustNum AND
                     Limit.LimitType = {&LIMIT_TYPE_TMRLIMIT} AND
                     Limit.TMRuleSeq = TMRule.TMRuleSeq AND
                     Limit.FromDate <= TODAY AND
                     Limit.ToDate >= TODAY :

                    add_double(tmcounter_struct,"limit_" + 
                       STRING(Limit.LimitID),Limit.LimitAmt).
            END.
         END.
         ELSE
            FOR EACH TMRLimit OF TMRule NO-LOCK WHERE
                     TMRLimit.FromDate <= TODAY AND
                     TMRLimit.ToDate   >= TODAY:

               IF TMRLimit.ValueType = 2 THEN
                  ldeLimitAmt = ((TMCounter.LimitAmt * TMRLimit.LimitPerc) / 100).
               ELSE ldeLimitAmt = TMRLimit.LimitAmt.

               add_double(tmcounter_struct,"limit_" +
                  STRING(TMRLimit.LimitID),ldeLimitAmt).
            END.
    END.
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
