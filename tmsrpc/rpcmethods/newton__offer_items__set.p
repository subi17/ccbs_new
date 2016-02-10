/**
 * Set offer items.
 *
 * @input  id;string;mandatory;offer item id
           offer_item;struct;offer item data
   @offer_item offer_id;optional;string;offer id
               amount;optional;double;amount of offer item
               valid_from;optional;datetime;valid from of the offer item
               valid_to;optional;datetime;valid to of the offer item
               display_in_ui;optional;boolean;
               display_on_invoice;optional;boolean;
               item_id;optional;string;
               item_type;optional;string;
               vat_included;optional;boolean;
               periods;optional;int;(eg: 1-12)
 * @output offer_item;struct; 
 */

{xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcId AS CHARACTER NO-UNDO.
DEFINE VARIABLE liId AS INTEGER NO-UNDO. 
DEFINE VARIABLE pcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcUserName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ocError AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llEqual AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lcRespStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liUpdatedOfferItemId AS INTEGER NO-UNDO. 

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcId = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").

lcstruct = validate_struct(pcStruct, "offer_id,amount,valid_from,valid_to,display_in_ui,display_on_invoice,item_id,item_type,vat_included,username!,periods").

IF gi_xmlrpc_error NE 0 THEN RETURN.

liId = INT(pcId) NO-ERROR.

IF ERROR-STATUS:ERROR THEN
   RETURN appl_err(SUBST("OfferItem id &1 is invalid ", pcId)).

FIND OfferItem WHERE OfferItem.OfferItemId = liId NO-LOCK NO-ERROR.
IF NOT AVAIL OfferItem THEN 
   RETURN appl_err(SUBST("OfferItem &1 not found", pcId)).

pcUserName = "VISTA_" + get_string(pcStruct, "username").

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

{Syst/commpaa.i}
gcBrand = "1".
katun = pcUserName.
{Syst/eventval.i}
{Syst/tmsconst.i}
{Mc/offer.i}
{rpcmethods/xmlrpc_names.i}

FUNCTION fCheckInvalidChangeWithOldTs RETURN CHARACTER:
    /* Here list all checks that make the change not allowed *
       especially in this situation */
    IF ttOfferItem.BeginStamp NE OfferItem.BeginStamp THEN
       RETURN "validfrom".
    IF ttOfferItem.Amount NE OfferItem.Amount THEN
       RETURN "amount".
    IF ttOfferItem.VatIncl NE OfferItem.VatIncl THEN
       RETURN "vatincl".
   RETURN "".
END.

lcRespStruct = add_struct(response_toplevel_id, "").

CREATE ttOfferItem.
BUFFER-COPY OfferItem TO ttOfferItem.
ASSIGN
   ttOfferItem.Offer         = get_string(pcStruct, "offer_id") 
                               WHEN LOOKUP("offer_id", lcStruct) > 0. 
ASSIGN
   ttOfferItem.ItemType = fConvertToTMSName(get_string(pcStruct, "item_type"))
                               WHEN LOOKUP("item_type", lcStruct) > 0.
ASSIGN
   ttOfferItem.ItemKey       = get_string(pcStruct, "item_id")
                               WHEN LOOKUP("item_id", lcStruct) > 0.

IF gi_xmlrpc_error NE 0 THEN DO:
   RETURN.
END.

ASSIGN
   ttOfferItem.EndStamp      = get_timestamp(pcStruct,"valid_to") 
                               WHEN LOOKUP("valid_to", lcStruct) > 0.

/* allow empty string and convert it to default value */
IF gi_xmlrpc_error = {&INVALID_METHOD_PARAMS} THEN DO:
   gi_xmlrpc_error = 0.
   gc_xmlrpc_error = "".
   IF get_string(pcStruct,"valid_to") EQ "" THEN DO:
      ttOfferItem.EndStamp = 20491231.86399. 
   END.
   ELSE DO:
      RETURN appl_err("Incorrect valid_to value: " + 
                      get_string(pcStruct,"valid_to")).
   END.
END.

ASSIGN
   ttOfferItem.Amount        = get_double(pcStruct, "amount")
                               WHEN LOOKUP("amount", lcStruct) > 0

   ttOfferItem.BeginStamp    = get_timestamp(pcStruct, "valid_from") 
                               WHEN LOOKUP("valid_from", lcStruct) > 0

   ttOfferItem.DispInUI      = get_bool(pcStruct, "display_in_ui")
                               WHEN LOOKUP("display_in_ui", lcStruct) > 0

   ttOfferItem.DispOnInvoice = get_bool(pcStruct, "display_on_invoice")
                               WHEN LOOKUP("display_on_invoice", lcStruct) > 0

   ttOfferItem.VatIncl       = get_bool(pcStruct, "vat_included")
                               WHEN LOOKUP("vat_included", lcStruct) > 0

   ttOfferItem.Periods       = get_int(pcStruct, "periods")
                               WHEN LOOKUP("periods", lcStruct) > 0.

IF gi_xmlrpc_error NE 0 THEN DO:
   RETURN.
END.

FIND Offer WHERE
     Offer.Brand = gcBrand AND 
     Offer.Offer = ttOfferItem.Offer NO-LOCK NO-ERROR.
IF NOT AVAIL Offer THEN DO:
   RETURN appl_err("Offer " + ttOfferItem.Offer + " does not exist").
END.

IF (OfferItem.BeginStamp < fMakeTs() OR 
   ttOfferItem.BeginStamp < fMakeTs()) AND Offer.FromDate <= TODAY THEN DO: 

   DEFINE VARIABLE cDeniedChangedInfo AS CHARACTER NO-UNDO. 
   cDeniedChangedInfo = fCheckInvalidChangeWithOldTs(). 
   IF cDeniedChangedInfo NE "" THEN DO:
      RETURN appl_err(
      "For OfferItem that has begun in history, " +
      cDeniedChangedInfo + " cannot be changed").
   END.
END.

IF ttOfferItem.ItemType = "Topup" THEN DO:

   IF ttOfferItem.Amount NE 0 THEN DO:
      ttOfferItem.Amount = 0.
      add_double(lcRespStruct, "amount", 0). 
   END.

   FIND FIRST TopupScheme WHERE 
      TopupScheme.Brand = gcBrand AND
      TopupScheme.TopupScheme = ttOfferItem.ItemKey NO-LOCK NO-ERROR.
   IF AVAIL TopupScheme THEN
   DO:
      IF ttOfferItem.VatIncl NE TopupScheme.VatIncl THEN
         add_boolean(lcRespStruct, "vat_included", TopupScheme.VatIncl).
      ttOfferItem.VatIncl = TopupScheme.VatIncl. 
   END.
END.

IF fValidateOfferItem(TABLE ttOfferItem, FALSE, OUTPUT ocError) > 0 THEN DO:
   RETURN appl_err(ocError).
END.

IF ttOfferItem.EndStamp < fMakeTs() THEN DO:
   ttOfferItem.EndStamp = fMakeTs().
   add_timestamp(lcRespStruct, "valid_to", ttOfferItem.EndStamp).
END.

BUFFER-COMPARE ttOfferItem TO OfferItem SAVE llEqual.
IF NOT llEqual THEN DO:

   FIND CURRENT OfferItem EXCLUSIVE-LOCK.

   IF llDoEvent THEN DO:
      &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
      {Func/lib/eventlog.i}
      DEF VAR lhOfferItem AS HANDLE NO-UNDO.
      lhOfferItem = BUFFER OfferItem:HANDLE.
      RUN StarEventInitialize(lhOfferItem).
      RUN StarEventSetOldBuffer(lhOfferItem).
   END.
   
   BUFFER-COPY ttOfferItem EXCEPT Brand OfferItemId TO OfferItem.

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhOfferItem).
      fCleanEventObjects().
   END.
END.

RELEASE OfferItem.

FINALLY:
   EMPTY TEMP-TABLE ttNamePairs.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
