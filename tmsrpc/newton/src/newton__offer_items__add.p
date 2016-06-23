/**
 * Add offer item
 *
 * @input   offer;struct;mandatory;offer item data
 * @offer   offer_id;string;mandatory;offer id
            amount;double;optional;amount for offer item
            valid_from;datetime;mandatory;valid from of the offer item
            valid_to;datetime;optional;valid to of the offer item
            display_in_ui;boolean;mandatory;
            display_on_invoice;boolean;mandatory;
            item_id;string;mandatory;
            item_type;string;mandatory;
            vat_included;boolean;mandatory;
            username;string;mandatory;
            periods;int;optional;
 * @output  ret_struct;struct;offer item id
   @ret_struct id;string;offer item id
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/eventval.i}
{Mc/offer.i}
{newton/src/xmlrpc_names.i}

DEFINE VARIABLE pcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcRespStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ocError AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liOfferItemId AS INTEGER NO-UNDO. 
DEFINE VARIABLE deCurTime AS DECIMAL NO-UNDO.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN DO:
   RETURN.
END.

lcStruct = validate_request(pcStruct, 
   "offer_id!,amount,valid_from!,valid_to,display_in_ui!," +
   "display_on_invoice!,item_id!,item_type!,vat_included!,username!,periods").
 
IF lcStruct = ? THEN DO:
   RETURN.
END.

katun = "VISTA_" + get_string(pcStruct, "username").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(katun) EQ "VISTA_" THEN DO:
   RETURN appl_err("username is empty").
END.

liOfferItemId = 1. 
FOR EACH OfferItem NO-LOCK BY OfferItem.OfferItemId DESC:
  liOfferItemId = OfferItem.OfferItemID + 1.
  LEAVE.
END.

CREATE ttOfferItem.
ttOfferItem.OfferItemId = liOfferItemId.
ttOfferItem.Brand = gcBrand.
ttOfferItem.Offer = get_string( pcStruct, "offer_id").
ttOfferItem.VatIncl = get_bool(   pcStruct, "vat_included").
ttOfferItem.Amount = ( IF LOOKUP("amount", lcStruct) > 0 THEN 
                      get_double( pcStruct, "amount") ELSE 0).
ttOfferItem.BeginStamp = get_timestamp( pcStruct, "valid_from").
ttOfferItem.EndStamp = (IF LOOKUP("valid_to", lcStruct) > 0 THEN 
                        get_timestamp(pcStruct,"valid_to") ELSE 20491231.86399).
ttOfferItem.DispInUI = get_bool(   pcStruct, "display_in_ui").
ttOfferItem.DispOnInvoice = get_bool(   pcStruct, "display_on_invoice").
ttOfferItem.ItemKey = get_string( pcStruct, "item_id").
ttOfferItem.ItemType = fConvertToTMSName(get_string( pcStruct, "item_type")).
ttOfferItem.VatIncl = get_bool(   pcStruct, "vat_included").

IF LOOKUP("periods", lcStruct) > 0 THEN
   ttOfferItem.Periods = get_int(pcStruct, "periods").

IF gi_xmlrpc_error NE 0 THEN DO:
   RETURN.
END.

IF fValidateOfferItem(TABLE ttOfferItem, TRUE, OUTPUT ocError) > 0 THEN DO:
   RETURN appl_err(ocError).
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
   {Func/lib/eventlog.i}
   DEF VAR lhOfferItem AS HANDLE NO-UNDO.
   lhOfferItem = BUFFER OfferItem:HANDLE.
   RUN StarEventInitialize(lhOfferItem).
END.

lcRespStruct = add_struct(response_toplevel_id, "").
add_string(lcRespStruct, "id", STRING(liOfferItemId)). 

IF ttOfferItem.ItemType = "Topup" THEN DO:
   
   IF ttOfferItem.Amount NE 0 THEN DO:
      ttOfferItem.Amount = 0.
      add_double(lcRespStruct, "amount", ttOfferItem.Amount). 
   END.

   FIND TopupScheme WHERE 
        TopupScheme.Brand = gcBrand AND
        TopupScheme.TopupScheme = ttOfferItem.ItemKey NO-LOCK NO-ERROR.
   
   IF AVAIL TopupScheme THEN DO:
      IF ttOfferItem.VatIncl NE TopupScheme.VatIncl THEN
         add_boolean(lcRespStruct, "vat_included", TopupScheme.VatIncl).
      ttOfferItem.VatIncl = TopupScheme.VatIncl. 
   END.
END.

deCurTime = fMakeTs().
IF ttOfferItem.BeginStamp < deCurTime THEN DO:
   ttOfferItem.BeginStamp = deCurTime.
   add_timestamp(lcRespStruct, "valid_from", ttOfferItem.BeginStamp).
END.

IF ttOfferItem.EndStamp < deCurTime THEN DO:
   ttOfferItem.EndStamp = deCurTime.
   add_timestamp(lcRespStruct, "valid_to", ttOfferItem.EndStamp).
END.

CREATE OfferItem.
BUFFER-COPY ttOfferItem TO OfferItem.
VALIDATE OfferItem.

IF llDoEvent THEN DO:
   RUN StarEventMakeCreateEvent (lhOfferItem).
   fCleanEventObjects().
END.

FINALLY:
   EMPTY TEMP-TABLE ttNamePairs.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
