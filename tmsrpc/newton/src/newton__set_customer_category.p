/* ----------------------------------------------------------------------
  Module .......: tmsrpc/newton/src/newton__add_pro_migration_request.p
  Task .........: Customer Category Change
  Application ..: RPCMETHOD
  Input ........:   Int;mandatory;CustNum
                    String;mandatory;TargetCategory
  Output .......:   success;boolean
  ---------------------------------------------------------------------- */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun   = "NewtonRPC".
{Syst/tmsconst.i}

IF validate_request(param_toplevel_id, "int,string") EQ ? THEN RETURN.

DEFINE VARIABLE liCustNum    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcCategory   AS CHARACTER NO-UNDO.

liCustNum   = get_int(param_toplevel_id, "0").
lcCategory  = get_string(param_toplevel_id, "1").

FIND Customer WHERE Customer.Custnum = liCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE customer THEN 
    RETURN appl_err(SUBST("Customer &1 not found", STRING(liCustNum))).

IF lcCategory = customer.category THEN  
    RETURN appl_err(SUBST("Customer is already in the same category")).

FIND CustCat WHERE CustCat.Category = lcCategory NO-LOCK NO-ERROR.
IF NOT AVAILABLE CustCat THEN     
    RETURN appl_err(SUBST("Invalid Category")).
    
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
            IF Mm.MManMessage:mGetMessage("SMS", "SVA_ActMessage", 1) EQ TRUE THEN DO:
                Mm.MManMessage:ParamKeyValue = "".
                Mm.MManMessage:mCreateMMLogSMS('').
                Mm.MManMessage:mClearData().
            END.
        END.
        RELEASE customer.
    END. 
    OTHERWISE DO: /* to SELF EMPLOYED */
        
    END.
END CASE.

add_boolean(response_toplevel_id, "", true).

 