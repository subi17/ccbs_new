/*


*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun   = "NewtonRPC".
{Syst/tmsconst.i}

IF validate_request(param_toplevel_id, "int,string") EQ ? THEN RETURN.

DEFINE VARIABLE lcCategory   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCustNum    AS INTEGER   NO-UNDO.

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
    WHEN "CONSUMER"        THEN DO TRANSACTION:
        FIND customer WHERE customer.Custnum = liCustNum EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE customer THEN 
            Customer.category = lcCategory.
        RELEASE customer.
    END. 
    OTHERWISE DO:
        
    END.
END CASE.

 