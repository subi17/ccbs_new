/**
 * Initial search for a terminated subscriptions.
 * Usually the method will return a list of subscriptions of which the given person is owner. When search with msisdn or imsi and multiple subscriptions are found, then the subscriptions with searched msisdn/imsi will be first in the array returned.
 * @input search_key;string;mandatory;search key value
         limit;int;mandatory;how many subscriptions to get at one time
         offset;int;mandatory;offset;how many subscriptions to skip over
         search_type;string;mandatory;msisdn,person_id,imsi,custnum
         admin;boolean;mandatory;admin user (admin has access to terminated subs older than 6 months)
 * @output subscriptions;array;contains subscription structs
           sub_count;int;total count of found subscriptions
 * @subscription seq;int;subscription ID
                 msisdn;string;MSISDN Number
                 subscription_type_id;string;subscription type (e.g. CONT2)
                 custnum;int;customer number of owner
                 name;string;full name of the owner
                 message;string;optional message (preactivated subs). 
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
katun = "Newton".
gcBrand = "1".
{newton/src/json_key.i}
{Func/timestamp.i}
{Mm/fbundle.i}

/* Input parameters */
DEF VAR pcInput AS CHAR NO-UNDO.
DEF VAR pcStruct AS CHAR NO-UNDO.
DEF VAR pcSearchType AS CHARACTER NO-UNDO. 
DEF VAR piOffSet AS INT NO-UNDO.
DEF VAR piLimit AS INT NO-UNDO.
DEF VAR plAdmin AS LOGICAL NO-UNDO.

/* Output parameters */
DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR result_array AS CHAR NO-UNDO.
DEF VAR sub_struct AS CHAR NO-UNDO.

/* Local variables */
DEF VAR lcParams AS CHAR NO-UNDO. 
DEF VAR liOwner AS INT NO-UNDO.
DEF VAR liSubCount AS INT NO-UNDO.
DEF VAR llSearchByMobsub AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.

DEFINE TEMP-TABLE ttOwner NO-UNDO
   FIELD Custnum AS INT
INDEX Custnum IS PRIMARY UNIQUE Custnum. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
validate_struct(pcStruct, "search_type!,search_key!,limit!,offset!,admin!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcSearchType = get_string(pcStruct, "search_type").
pcInput = get_string(pcStruct, "search_key").
plAdmin = get_bool(pcStruct, "admin").

IF pcSearchType EQ "custnum" THEN DO:
   liOwner = INT(pcInput) NO-ERROR.
   IF ERROR-STATUS:ERROR OR liOwner <= 0 THEN RETURN
      appl_err("incorrect search_key").
END.

piLimit  = get_pos_int(pcStruct, "limit").
piOffSet = get_int(pcStruct, "offset").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

FUNCTION fAddSubStruct RETURNS LOGICAL:

   DEF VAR lcBundle AS CHAR NO-UNDO. 

   sub_struct = add_json_key_struct(result_array, "").
   add_int(sub_struct   , "seq" , TermMobSub.msseq).
   add_string(sub_struct, "msisdn", TermMobSub.Cli).
   add_string(sub_struct, "subscription_type_id", TermMobSub.CliType).
   add_int(sub_struct, "custnum", Customer.CustNum).
   add_string(sub_struct, "name", SUBST("&1 &2 &3", Customer.FirstName,
                                                 Customer.CustName,
                                                 Customer.Surname2)).
    
   IF LOOKUP(TermMobsub.CliType,lcBundleCLITypes) > 0 THEN DO:
      IF TermMobsub.TariffBundle > "" THEN
         lcBundle = TermMobsub.TariffBundle.
      ELSE
         lcBundle = fGetTerminatedSpecificBundle(TermMobsub.MsSeq,
                                                 fMakeTS(),
                                                 TermMobsub.CliType).
   END.

   add_string(sub_struct, "data_bundle_id", lcBundle).

   /* additional message for Newton to display for user */
   IF Customer.SalesMan EQ "PRE-ACT" THEN
      add_string(sub_struct, "message",
                             "MSIDSN listing is disabled for this customer"). 
   
END FUNCTION. 

FUNCTION fIsViewableTermMobsub RETURNS LOGICAL
   (iiMsSeq AS INTEGER):

   DEF VAR ldaDate AS DATE NO-UNDO. 
   DEF VAR liTime AS INT NO-UNDO. 
   DEF BUFFER MsOwner FOR MsOwner.
   
   FIND FIRST Msowner WHERE 
              Msowner.msseq = iiMsSeq
   NO-LOCK USE-INDEX MsSeq NO-ERROR.
   IF NOT AVAIL Msowner THEN RETURN FALSE.
   
   fSplitTS(msowner.tsend, output ldaDate, output liTime).
   IF TODAY - ldaDate > 180 AND NOT plAdmin THEN RETURN FALSE.

   RETURN TRUE.

END FUNCTION. 

top_struct = add_struct(response_toplevel_id, "").
result_array = add_array(top_struct, "subscriptions").

IF pcSearchType EQ "msisdn" THEN DO:
   
   RELEASE ttOwner.
   FOR EACH termmobsub NO-LOCK WHERE
      termmobsub.cli = pcInput AND
      termmobsub.brand = gcBrand,
      FIRST Customer NO-LOCK WHERE
            Customer.Custnum = TermMobSub.Custnum: 
   
      IF NOT fIsViewableTermMobsub(TermMobSub.MsSeq) THEN NEXT.
      
      IF piOffSet = 0 OR Customer.Salesman EQ "PRE-ACT" THEN DO: 
         fAddSubStruct().
         liSubCount = liSubCount + 1.
         NEXT.
      END.
      
      FIND FIRST ttOwner NO-LOCK where
                 ttOwner.Custnum = termmobsub.Custnum NO-ERROR.
      IF AVAIL ttOwner THEN NEXT.

      CREATE ttOwner.
      ASSIGN
         ttOwner.Custnum = TermMobsub.Custnum.
   END.
   
   IF liSubCount = 0 AND NOT AVAIL ttOwner THEN 
       RETURN appl_err(SUBST("MobSub entry &1 not found", pcInput)).
   
   llSearchByMobsub = TRUE.
END. 
ELSE IF pcSearchType EQ "imsi" THEN DO:
   
   RELEASE ttOwner.
   FOR EACH TermMobsub NO-LOCK WHERE
      TermMobSub.Brand = gcBrand AND
      TermMobSub.IMSI  = pcInput,
      FIRST Customer NO-LOCK WHERE
            Customer.Brand = gcBrand AND
            Customer.Custnum = TermMobSub.Custnum: 
   
      IF NOT fIsViewableTermMobsub(TermMobSub.MsSeq) THEN NEXT.
      
      IF piOffSet = 0 OR Customer.Salesman EQ "PRE-ACT" THEN DO: 
         fAddSubStruct().
         liSubCount = liSubCount + 1.
         NEXT.
      END.
      
      FIND FIRST ttOwner NO-LOCK where
                 ttOwner.Custnum = termmobsub.Custnum NO-ERROR.
      IF AVAIL ttOwner THEN NEXT.

      CREATE ttOwner.
      ASSIGN
         ttOwner.Custnum = TermMobsub.Custnum.
   END.
   
   IF liSubCount = 0 AND NOT AVAIL ttOwner THEN 
      RETURN appl_err(SUBST("IMSI &1 not found", pcInput)).

   llSearchByMobsub = TRUE.

END.
ELSE IF pcSearchType EQ "custnum" AND liOwner NE 0 THEN DO:

   FIND Customer NO-LOCK WHERE
        Customer.CustNum = liOwner AND
        Customer.brand = gcBrand NO-ERROR.
   IF NOT AVAILABLE Customer THEN
      RETURN appl_err(SUBST("Customer &1 not found 1", liOwner)).
   
   IF Customer.Salesman = "PRE-ACT" THEN 
      RETURN appl_err("Please search for MSISDN only").

   CREATE ttOwner.
   ASSIGN
      ttOwner.Custnum = Customer.Custnum.
END.
ELSE IF pcSearchType EQ "person_id" THEN DO:
   
   FIND Customer NO-LOCK WHERE
        Customer.brand = gcBrand AND
        Customer.OrgId = pcInput AND
        Customer.Roles NE "inactive" USE-INDEX OrgId NO-ERROR.

   IF AMBIGUOUS(Customer) THEN
      RETURN appl_err("Please search with MSISDN or customer number").
   ELSE IF NOT AVAIL Customer THEN
      RETURN appl_err(SUBST("Customer &1 not found 2", pcInput)).
   
   IF Customer.Salesman = "PRE-ACT" THEN 
      RETURN appl_err("Please search for MSISDN only").

   CREATE ttOwner.
   ASSIGN
      ttOwner.Custnum = Customer.CustNum.
END.
ELSE RETURN appl_err(SUBST("Unknown search type &1", pcSearchType)).

FOR EACH ttOwner NO-LOCK,
    EACH TermMobSub NO-LOCK WHERE
         TermMobSub.Custnum = ttOwner.Custnum AND
         TermMobSub.brand = gcBrand,
    FIRST Customer NO-LOCK WHERE
          Customer.brand = gcBrand AND
          Customer.custnum = TermMobSub.custnum:
     
    IF llSearchByMobsub AND (pcInput EQ TermMobSub.CLI OR
                             pcInput EQ TermMobSub.IMSI) THEN NEXT.

    IF NOT fIsViewableTermMobsub(TermMobSub.MsSeq) THEN NEXT.
    
    liSubCount = liSubCount + 1.
    IF liSubCount <= piOffSet THEN NEXT.
    IF liSubCount > (piOffSet + piLimit) THEN NEXT. 
    
    fAddSubStruct().
END.

IF sub_struct = '' THEN RETURN appl_err("Subscriptions not found").

add_int(top_struct, "sub_count", liSubCount).

FINALLY:
   EMPTY TEMP-TABLE ttOwner.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
