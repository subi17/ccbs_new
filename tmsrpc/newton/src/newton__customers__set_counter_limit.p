/**
 * Set tmt counter limits
 *
 * @input  custnum;int;mandatory;customer id
           username;string;mandatory; user that execute change
           tmruleseq;int;mandatory; counter rule sequence
           limitId;int;mandatory;limit Id
           amount;decimal;mandatory; limit amount

*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}


DEFINE VARIABLE piCustNum AS INTEGER NO-UNDO. 
DEFINE VARIABLE pcUserName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE piTMRuleSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE piLimitId AS INTEGER NO-UNDO. 
DEFINE VARIABLE pdLimitAmt AS DECIMAL NO-UNDO.

DEFINE VARIABLE liMsReq  AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldMinValue      AS DECIMAL NO-UNDO EXTENT 2.  
DEFINE VARIABLE ldMaxValue      AS DECIMAL NO-UNDO EXTENT 2.  
DEFINE VARIABLE ldValue     AS DECIMAL NO-UNDO EXTENT 2.  
DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ic AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcBrand AS CHARACTER NO-UNDO INITIAL "1".
DEFINE VARIABLE lcMode AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "int,string,int,int,double") EQ ? THEN RETURN.

ASSIGN
   piCustNum = get_int(param_toplevel_id, "0")
   pcUserName = "VISTA_" + get_string(param_toplevel_id, "1")
   piTMRuleSeq = get_int(param_toplevel_id,"2")
   piLimitId = get_int(param_toplevel_id,"3")
   pdLimitAmt = get_double(param_toplevel_id,"4").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

/* find the corresponding TMRule */
FIND TMRule WHERE 
     TMRule.TMRuleSeq = piTMRuleSeq NO-LOCK NO-ERROR.

IF NOT AVAIL TMRule THEN 
   RETURN appl_err("TMRule doesn't exist").

IF TMRule.LimitSource NE 1 AND TMRule.LimitSource NE 4 THEN 
   RETURN appl_err("Not a customer based limit").

/* find now the limit if it exist */
FIND FIRST Limit WHERE
           Limit.Custnum = piCustNum AND
           Limit.LimitType = 1 AND
           Limit.TMRuleSeq = piTMRuleSeq
      USE-INDEX CustNum NO-LOCK NO-ERROR.
IF NOT AVAIL Limit THEN DO:
   IF TMRule.LimitSource = 4 THEN lcMode = "Create".
   ELSE RETURN appl_err("There is not customer based limit " 
                      + "defined for that counter").
END.
ELSE lcMode = "Update".

/* get max and min values in TMRLimit */
FOR EACH TMRLimit WHERE
         TMRLimit.TMRuleSeq = TMRule.TMRuleSeq AND
         TMRLimit.ToDate   >= TODAY AND
         TMRLimit.FromDate <= TODAY NO-LOCK USE-INDEX LimitId :
         
   ldMinValue[TMRLimit.LimitId] = TMRLimit.MinValue.
   ldMaxValue[TMRLimit.LimitId] = TMRLimit.MaxValue.
   IF TMRule.LimitSource = 4 THEN 
      ldValue[TMRLimit.LimitID] = TMRLimit.LimitAmt.
END.
      
/* get current values */
FOR EACH Limit WHERE
         Limit.Custnum = piCustnum AND
         Limit.LimitType = 1 AND
         Limit.TMRuleSeq = TMRule.TMRuleSeq AND
         Limit.ToDate >= TODAY  AND
         Limit.FromDate <= TODAY NO-LOCK :
   ldValue[Limit.LimitId] = Limit.LimitAmt.
END.
/* get input value */
ldValue[piLimitId] = pdLimitAmt .

/* apply bussines rules */
IF ldValue[2] < ldValue[1] THEN DO:
   RETURN appl_err("Values not allowed due to business rules:" +
                  "  Limit 1 " +  STRING(ldValue[1]) +
                  "  > Limit 2 " + STRING(ldValue[2]) ).
END.
DO ic = 1 TO 2:
     IF ldValue[ic] > ldMaxValue[ic] OR
        ldValue[ic] < ldMinValue[ic] THEN 
        RETURN appl_err("Value not allowed due to business rules:" +
                       " maxValue " + STRING(ldMaxValue[ic]) +
                       " minValue " + STRING(ldMinValue[ic]) ).
END.

{Syst/commpaa.i}
katun = pcUserName.
gcBrand = lcBrand.
{Func/flimitreq.i}

/* ready to create the request */
liMsReq = fLimitRequest(
          ?,           /* msseq */
          piCustnum,   /* custum */
          fMakeTS(),   /* act.stamp */ 
          lcMode,    /* create, update */
          ldValue,  /* new limit values */
          FALSE,       /* default value */ 
          TMRule.TMRuleSeq, /* tmruleseq */
          1,           /* limit type */
          "6",         /* source of request  - Manual event web */
          OUTPUT ocResult).

IF liMsReq = 0 THEN RETURN appl_err(ocResult).

add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
