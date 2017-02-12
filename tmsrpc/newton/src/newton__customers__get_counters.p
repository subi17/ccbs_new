/**
 * Get tmt counter limits
 *
 * @input int;mandatory;customer id

 * @output tmcounter;array of TMT counters for that customer;
           request;boolean; indicate if there is a current request for TMT limits

 * @tmcounter name;string;counter rule name
              id;integer;counter rule id
              change;boolean;if is allowed to change the limits
              limits;array of limits
 * @limit     id;integer;limitid
              name;string; limit id name
              unit_type;string;units type
              amount;decimal;amount
              valid_from;date; valid from date
              valid_to;date; valid to date
              default;boolean; default value

*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Func/timestamp.i}
{Func/dataformat.i}

DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
DEFINE VARIABLE resp_struct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lctmcounters AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lctmcounter_struct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lclimits AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lclimit_struct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE piCustNum AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcValueType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcValue AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liLimitSource AS INTEGER NO-UNDO. 


FUNCTION fAddLimitStruct RETURN LOGICAL
         (INPUT piLimitId   AS INTEGER,
          INPUT pdLimitAmt AS DECIMAL,
          INPUT pdFromDate AS DATE,
          INPUT pdToDate   AS DATE,
          INPUT plDefValue AS LOGICAL):

   lclimit_struct = add_struct(lclimits, "").
   add_int(lclimit_struct,"id",piLimitID).
   add_string(lclimit_struct,"name","Limit " + STRING(piLimitID)).
   add_string(lclimit_struct,"unit_type",lcValueType).

   add_double(lclimit_struct,"amount",pdLimitAmt).
   add_datetime(lclimit_struct,"valid_from",pdFromDate).
   IF pdToDate < 1/19/2038 THEN 
   add_datetime(lclimit_struct,"valid_to",pdToDate).

   add_boolean(lclimit_struct,"default",plDefValue).

END FUNCTION.


/* MAIN */
IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piCustNum = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO common Customer CustNum piCustNum}

resp_struct = add_struct(response_toplevel_id, "").

/* Check ongoing limit requests */
FIND FIRST MsRequest WHERE
           MsRequest.Brand = gcBrand AND
           MsRequest.Reqtype = 40 AND
           MsRequest.CustNum = piCustnum AND
           LOOKUP(STRING(MsRequest.ReqStatus),"0,1,3") >  0 
           NO-LOCK NO-ERROR.

IF AVAIL MsRequest THEN 
      add_boolean(resp_struct,"request",TRUE).
ELSE 
      add_boolean(resp_struct,"request",FALSE).


lctmcounters = add_array(resp_struct,"tmcounters").
FOR EACH TMRule  NO-LOCK WHERE
         TMRule.Brand = gcBrand AND
         TMRule.FromDate <= TODAY AND
         TMRule.ToDate >=  TODAY :

         lctmcounter_struct = add_struct(lctmcounters,"").
         add_int(lctmcounter_struct,"id",TMRule.TMRuleSeq).
         add_string(lctmcounter_struct,"name",TMRule.Name).
         lclimits = add_array(lctmcounter_struct,"limits").
         
         /* use customer limit if it is found, otherwise use default limit */
         IF TMRule.LimitSource = 4 THEN DO:
            IF CAN-FIND(FIRST Limit NO-LOCK USE-INDEX CustNum WHERE
                              Limit.CustNum   = piCustNum AND
                              Limit.LimitType = 1 AND
                              Limit.TMRuleSeq = TMRule.TMRuleSeq AND
                              Limit.ToDate   >= TODAY AND
                              Limit.FromDate <= TODAY)
            THEN liLimitSource = 1.
            ELSE liLimitSource = 3.
         END.
         ELSE liLimitSource = TMRule.LimitSource.
       
         IF liLimitSource NE 3 THEN DO:
             add_boolean(lctmcounter_struct,"change",TRUE).
             FOR EACH Limit NO-LOCK WHERE
                      Limit.CustNum = piCustNum AND
                      Limit.LimitType = 1 AND
                      Limit.TMRuleSeq = TMRule.TMRuleSeq:

                      RUN fFormatUnit(
                             TMRule.CounterAmount,
                             Limit.LimitAmt,
                             OUTPUT lcValueType,
                             OUTPUT lcValue). 

                      fAddLimitStruct(Limit.LimitId,
                                        Limit.LimitAmt,
                                        Limit.FromDate,
                                        Limit.ToDate,
                                        Limit.DefValue).
             END.
         END.
         ELSE DO:
            add_boolean(lctmcounter_struct,"change", 
                        (IF TMRule.LimitSource NE 4 THEN FALSE ELSE TRUE)).
            FOR EACH TMRLimit OF TMRule NO-LOCK :

                      RUN fFormatUnit(
                             TMRule.CounterAmount,
                             TMRLimit.LimitAmt,
                             OUTPUT lcValueType,
                             OUTPUT lcValue). 

                      fAddLimitStruct(TMRLimit.LimitId,
                                        TMRLimit.LimitAmt,
                                        TMRLimit.FromDate,
                                        TMRLimit.ToDate,
                                        TRUE).
            END.
         END.
END.

