/**
 * Returns customer's MGM (Member Gets Member) commissions
 *
 * @input       custnum;int;customer number
 * @output      commission[];array of structs;array of commission structs
 * @commission  msisdn;string;msisdn of committed subscription
                status;string;commission status (PENDING, ACTIVATED or REJECTED)
                status_reason;int;commission status reason code
                commission_total;double;total commission
                commission_divided;double;for how many months commission is divided
                order_date;date;date when order is created
                activation_date;date;date when commission is created
                creation_date;date;date when commission is handled
                firstname;string;promoted person's firstname
                topups[];array of structs;array of commission related topups
                fatimes;struct;commission related fatimes
 * @topups      amount;double;topup amount 
                created;date;when topup is (going to be) created
                status;int;2 = ok, else not handled or error 
 * @fatimes     amount;double;FAT amount
                used;double;how much FAT amount is used
                period;date;period (year, month) of FAT
 */
{xmlrpc/xmlrpc_access.i}
{Func/date.i}
DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".

/* Input parameters */
DEF VAR piCustNum AS INT NO-UNDO.
/* Output parameters */

DEF VAR resp_array AS CHAR NO-UNDO.
DEF VAR resp_mobsub_struct AS CHAR NO-UNDO.
DEF VAR resp_array_of_structs AS CHAR NO-UNDO.
DEF VAR resp_struct AS CHAR NO-UNDO.
DEF VAR topup_array AS CHAR NO-UNDO.
DEF VAR topup_struct AS CHAR NO-UNDO.
DEF VAR fatime_array AS CHAR NO-UNDO.
DEF VAR fatime_struct AS CHAR NO-UNDO.
/* Local variables */
DEF VAR liSecs AS INTEGER NO-UNDO.
DEF VAR ldePaid AS DECIMAL NO-UNDO.
DEF VAR ldeFatime AS DECIMAL NO-UNDO.
DEF VAR ldaFatPeriod AS DATE NO-UNDO.
DEF VAR lcStatus AS CHARACTER NO-UNDO.
DEF VAR liRefCustnum AS INT NO-UNDO. 

DEF BUFFER bRefMobSub FOR Mobsub.
DEF BUFFER bRefCustomer FOR Customer.
DEF BUFFER fatime2 FOR Fatime.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piCustNum = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND Customer WHERE 
     Customer.Brand  = gcBrand AND 
     Customer.CustNum = piCustNum 
     NO-LOCK NO-ERROR.

IF NOT AVAIL Customer THEN
   appl_err(SUBST("Customer &1 not found", piCustNum)).

resp_array = add_array(response_toplevel_id, "").

FOR EACH mobsub NO-LOCK WHERE
   mobsub.brand = gcBrand AND
   mobsub.Custnum = piCustNum,
   EACH COTarg WHERE
        COTarg.Brand      = gcBrand AND
        COTarg.TargType   = "M" AND
        COTarg.CoTarg     = STRING(Mobsub.MsSeq) NO-LOCK,
      FIRST CORule OF COTarg NO-LOCK
      BREAK BY COTarg.COTarg:

   IF FIRST-OF(COTarg.COTarg) THEN DO:
      resp_mobsub_struct = add_struct(resp_array, "").
      add_string(resp_mobsub_struct, "msisdn", mobsub.cli).
      resp_array_of_structs = add_array(resp_mobsub_struct, "commissions").
   END.
   
   lcStatus = "".
   lcStatus = ENTRY(COTarg.CommStatus,"PENDING,ACTIVATED,REJECTED") NO-ERROR.
   IF lcStatus = "" THEN lcStatus = STRING(CoTarg.CommStatus).
   
   resp_struct = add_struct(resp_array_of_structs, "").
   add_string(resp_struct, "msisdn", COTarg.PromotedCLI).
   add_string(resp_struct, "type", (IF COTarg.PromotedId > 0 THEN "REFEREE" ELSE "PROMOTED")).
   add_string(resp_struct, "status", lcStatus).
   add_int(resp_struct, "status_reason", COTarg.StatusReason).
   add_double(resp_struct, "commission_total", CORule.CommAmount).
   add_double(resp_struct, "commission_divided", CORule.CoNoInst).
   
   FIND FIRST Order WHERE 
      Order.Brand   = gcBrand AND
      Order.OrderId = COTarg.OrderId NO-LOCK NO-ERROR.
   
   IF AVAIL Order THEN
      add_timestamp(resp_struct, "order_date", Order.CrStamp).
  
   IF COTarg.PromotedId > 0 THEN DO:
      
      liRefCustnum = 0. 
      
      FIND bRefMobSub WHERE 
         bRefMobSub.MsSeq = COTarg.PromotedID NO-LOCK NO-ERROR.
      
      IF AVAIL bRefMobSub THEN DO:
         liRefCustnum = bRefMobSub.Custnum.
      END.
      ELSE DO: 
         FIND TermMobSub WHERE 
            TermMobSub.MsSeq = COTarg.PromotedID NO-LOCK NO-ERROR.
         IF AVAIL TermMobSub THEN liRefCustnum = TermMobSub.Custnum. 
      END.
      
      IF liRefCustnum > 0 THEN DO:
         FIND FIRST bRefCustomer WHERE 
            bRefCustomer.Custnum = liRefCustnum NO-LOCK NO-ERROR.
      END.
   END.
   
   IF AVAIL bRefCustomer THEN 
      add_string(resp_struct, "firstname", bRefCustomer.firstname).
   ELSE 
      add_string(resp_struct, "firstname", "").

   RELEASE bRefCustomer.

   add_timestamp(resp_struct, "creation_date", COTarg.CreatedTS).
   
   IF COTarg.HandledTS > 0 THEN DO:
      add_timestamp(resp_struct, "activation_date", COTarg.HandledTS).
   END.
       
   /* Fetch commission topups and fats */
   CASE CORule.PayType:
   
      WHEN 1 THEN DO: /* Postpaid */
         
         fatime_array = add_array(resp_struct, "fatimes").
         
         FOR EACH Fatime WHERE
            Fatime.Brand   = gcBrand AND
            Fatime.HostTable = "COTarg" AND
            Fatime.KeyValue = STRING(COTarg.COTargId) AND
            Fatime.OrigFat = 0 AND
            Fatime.InvNum NE 1 NO-LOCK:
         
            fatime_struct = add_struct(fatime_array, "").
        
            ldaFATPeriod = date((fatime.period mod 100), 
                                 1, 
                                 int(fatime.period / 100)) no-error.

            add_date_or_time(fatime_struct, "period", ldaFATPeriod, 0).         
            add_double(fatime_struct, "amount", Fatime.Amt).    

            /* Count total used FAT */
            ldeFatime = 0.
            FOR EACH Fatime2 WHERE
               Fatime2.Brand     = gcBrand AND
               Fatime2.HostTable = "COTarg" AND
               Fatime2.KeyValue  = STRING(COTarg.COTargId) AND
               Fatime2.Period    = Fatime.Period AND
               Fatime2.InvNum NE 1 NO-LOCK:
               
               ldeFatime = ldeFatime + Fatime2.Used.
            END.
            
            add_double(fatime_struct, "used", ldeFatime).    

         END.

      END. 

      WHEN 2 THEN DO: /* Prepaid */
         
         topup_array = add_array(resp_struct, "topups").
         
         FOR EACH PrePaidRequest WHERE
            PrepaidRequest.Brand       = gcBrand AND
            PrePaidRequest.Reference   = STRING(COTarg.COTargId) AND
            PrePaidRequest.Request     = "RefillTRequest" AND
            PrePaidRequest.Source      = CoRule.PPSource NO-LOCK:
         
            topup_struct = add_struct(topup_array, "").
            
            add_double(topup_struct, "amount", PrepaidRequest.TopUpAmt / 100).  
            add_timestamp(topup_struct, "created", PrepaidRequest.TSRequest). 
            add_int(topup_struct, "status", PrepaidRequest.PPStatus).
         END.
      END. 
   
   END. /* CASE */
END.
