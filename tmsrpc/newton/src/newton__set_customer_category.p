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
{Syst/eventlog.i}
{Syst/eventval.i}

IF validate_request(param_toplevel_id, "int,string,string") EQ ? THEN RETURN.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
   DEF VAR lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).
END.


DEFINE VARIABLE liCustNum    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcMSISDN     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCategory   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcError      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liRequest    AS INTEGER   NO-UNDO.
DEFINE BUFFER bf_CustCat FOR CustCat.
liCustNum   = get_int(param_toplevel_id, "0").
lcMSISDN    = get_string(param_toplevel_id, "1").
lcCategory  = get_string(param_toplevel_id, "2").

FIND Customer WHERE Customer.Brand = Syst.Var:gcBrand AND Customer.Custnum = liCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE customer THEN 
    RETURN appl_err(SUBST("Customer &1 not found", STRING(liCustNum))).


FIND FIRST CustCat NO-LOCK WHERE
           Custcat.brand EQ Syst.Var:gcBrand AND
           CustCat.Category = Customer.Category NO-ERROR.

IF NOT AVAILABLE CustCat THEN     
    RETURN appl_err(SUBST("Invalid Category")).

IF lcCategory = CustCat.Segment THEN  
    RETURN appl_err(SUBST("Customer is already in the same category")).
    
FIND FIRST bf_CustCat NO-LOCK WHERE
           bf_CustCat.brand EQ Syst.Var:gcBrand AND
           bf_CustCat.CustIdType EQ Customer.CustIdType AND 
           bf_CustCat.segment EQ lcCategory NO-ERROR.

IF NOT AVAILABLE bf_CustCat THEN     
    RETURN appl_err(SUBST("Invalid Category")).
    
FIND MobSub WHERE 
     Mobsub.Brand = Syst.Var:gcBrand AND 
     MobSub.Cli = lcMSISDN AND 
     MobSub.CustNum = Customer.CustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN     
    RETURN appl_err(SUBST("Subscription not found")).
     
CASE lcCategory:
    WHEN "CONSUMER" THEN DO TRANSACTION:
        FIND customer WHERE customer.Custnum = liCustNum EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE customer THEN DO:
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).
            Customer.category = bf_CustCat.category.
            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustomer).
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
            MobSub.Msseq     ,
            Customer.CustNum ,
            bf_CustCat.category ,
            Customer.category,
            lcMSISDN         ,
            {&REQUEST_SOURCE_NEWTON} ,
            OUTPUT lcError  
            ).
         
        IF liRequest = 0 OR liRequest = ? OR lcError NE "" THEN 
            RETURN appl_err(SUBST("Category request failed. &1" , lcError)).

    END.
    
END CASE.

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
END.
