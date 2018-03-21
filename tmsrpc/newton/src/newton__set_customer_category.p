/* ----------------------------------------------------------------------
  Module .......: tmsrpc/newton/src/newton__add_pro_migration_request.p
  Task .........: Customer Category Change
  Application ..: RPCMETHOD
  Input ........:   Int;mandatory;CustNum
                    String;mandatory;Cli
                    String;mandatory;TargetCategory
  Output .......:   success;boolean
  ---------------------------------------------------------------------- */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun   = "NewtonRPC".
{Syst/tmsconst.i}
{Func/fmakemsreq.i}

IF validate_request(param_toplevel_id, "int,string,string") EQ ? THEN RETURN.

DEFINE VARIABLE liCustNum    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcMSISDN     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCategory   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcError      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liRequest    AS INTEGER   NO-UNDO.

liCustNum   = get_int(param_toplevel_id, "0").
lcMSISDN    = get_string(param_toplevel_id, "1").
lcCategory  = get_string(param_toplevel_id, "2").

FIND Customer WHERE Customer.Brand = Syst.Var:gcBrand AND Customer.Custnum = liCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE customer THEN 
    RETURN appl_err(SUBST("Customer &1 not found", STRING(liCustNum))).

IF lcCategory = customer.category THEN  
    RETURN appl_err(SUBST("Customer is already in the same category")).

FIND CustCat WHERE CustCat.Category = lcCategory NO-LOCK NO-ERROR.
IF NOT AVAILABLE CustCat THEN     
    RETURN appl_err(SUBST("Invalid Category")).
FIND MobSub WHERE 
     Mobsub.Brand = Syst.Var:gcBrand AND 
     MobSub.Cli = lcMSISDN AND 
     MobSub.CustNum = Customer.CustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN     
    RETURN appl_err(SUBST("Subscription not found")).
     
CASE CustCat.Segment:
    WHEN "CONSUMER" THEN DO TRANSACTION:
        FIND customer WHERE customer.Custnum = liCustNum EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE customer THEN DO: 
            Customer.category = lcCategory.
            CREATE Memo.
            ASSIGN
                Memo.CreStamp  = {&nowTS}
                Memo.Brand     = Syst.Var:gcBrand
                Memo.HostTable = "Customer"
                Memo.KeyValue  = STRING(Customer.Custnum)
                Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
                Memo.CreUser   = Syst.Var:katun
                Memo.CustNum   = Customer.Custnum
                Memo.MemoTitle = "Category Change"
                Memo.MemoText  = "Cambio de categoría – De RESIDENCIAL AUTÓNOMO a PARTICULAR. Solicitado por el cliente".
        END.
        RELEASE customer.
    END. 
    OTHERWISE DO: /* to SELF EMPLOYED */

        liRequest = fCustomerCategoryChangeRequest ( ? ,
            Syst.Var:katun   ,
            Customer.CustNum ,
            MobSub.Msseq     ,
            lcCategory       ,
            Customer.category,
            lcMSISDN         ,
            {&REQUEST_SOURCE_NEWTON} ,
            OUTPUT lcError  
            ).
         
        IF liRequest = 0 OR liRequest = ? OR lcError NE "" THEN 
            RETURN appl_err(SUBST("Category request failed. &1" , lcError)).
            
        IF Mm.MManMessage:mGetMessage("SMS", "CategoryChangeSMS", 1) EQ TRUE THEN DO:
            Mm.MManMessage:ParamKeyValue = "".
            Mm.MManMessage:mCreateMMLogSMS('').
            Mm.MManMessage:mClearData().
        END.
        
    END.
    
END CASE.

add_boolean(response_toplevel_id, "", TRUE).

 