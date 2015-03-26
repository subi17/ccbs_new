/**
   Set billing item
   @ billitem_id;mandatory;string
     billitem_info;mandatory;struct
   @ billitem_info  ui_order;int; not mandatory;order number according to 
                     which billitems are listed in ui
                    active;boolean;optional;is (terminal) billitem active
                    username;string;mandatory;username of the changer of 
                    the billitem for eventlog
                    name;string;not mandatory; billing item name
                    title_es;optional;
                    title_ca;optional;
                    title_eu;optional;
                    title_ga;optional;
                    title_en;optional;
*/

{xmlrpc/xmlrpc_access.i}


DEFINE VARIABLE pcId          AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUserName    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStruct      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStruct      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE licount      AS INTEGER NO-UNDO. 
DEFINE VARIABLE lctitle   AS CHARACTER NO-UNDO EXTENT 5 
                          INITIAL ["title_es","title_ca","title_eu","title_ga","title_en"] . 
DEFINE VARIABLE lcBrand     AS CHARACTER NO-UNDO INITIAL "1".

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcID = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").

lcStruct = validate_struct(pcStruct, "ui_order,active,username!,name," + 
                                     "title_es,title_ca,title_eu,title_ga,title_en").
 
pcUserName = "VISTA_" + get_string(pcStruct, "username").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

{commpaa.i}
gcBrand = lcBrand.

FIND BillItem WHERE 
     BillItem.Brand = gcBrand AND 
     BillItem.BillCode = pcId EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF NOT AVAIL BillItem THEN
DO:
   IF LOCKED BillItem THEN DO:
      RETURN appl_err("Billing Item " + pcId + " is locked by other user, please try late ").
   END.
   ELSE DO:
      RETURN appl_err("Billing Item " + pcId + " has been deleted by other user ").
   END.
END.

katun = pcUserName.
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
   {lib/eventlog.i}
   DEF VAR lhBillItem AS HANDLE NO-UNDO.
   lhBillItem = BUFFER BillItem:HANDLE.
   RUN StarEventInitialize(lhBillItem).
   RUN StarEventSetOldBuffer(lhBillItem).
END.

ASSIGN
   BillItem.OrderChannelOrder = get_int(pcStruct, "ui_order")
      WHEN LOOKUP("ui_order",lcStruct) > 0
   BillItem.BIName = get_string(pcStruct, "name")
      WHEN LOOKUP("name",lcStruct) > 0
   BillItem.Active = get_bool(pcStruct, "active")
      WHEN LOOKUP("active",lcStruct) > 0.

IF llDoEvent THEN DO:
   RUN StarEventMakeModifyEvent(lhBillItem).
   fCleanEventObjects().
END.

RELEASE BillItem.

/* set/add/delete translations names */

IF llDoEvent THEN DO:
   DEF VAR lhRepText AS HANDLE NO-UNDO.
   lhRepText = BUFFER RepText:HANDLE.
   RUN StarEventInitialize(lhRepText).
END.

DO licount = 1 TO 5 :

   IF LOOKUP(lctitle[licount],lcStruct) = 0 THEN NEXT.
   
   DEFINE VARIABLE lctxt AS CHARACTER NO-UNDO. 
   lctxt = get_string(pcStruct,lctitle[licount]).

   FIND RepText NO-LOCK  WHERE 
        RepText.Brand     = lcBrand AND
        RepText.TextType  = 1       AND
        RepText.LinkCode  = pcId    AND
        RepText.Language  = licount AND
        repText.ToDate >= TODAY NO-ERROR.


   IF AVAIL RepText THEN DO:
       FIND CURRENT RepText EXCLUSIVE-LOCK NO-ERROR.
       IF lctxt = "" THEN DO:
            /* delete */    
            RUN StarEventMakeDeleteEvent (lhRepText). 
            DELETE RepText.
       END.
       ELSE DO:
           /* update */
           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRepText). 

           ASSIGN RepText.RepText   =  lctxt.
           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRepText).

       END.

   END.
   ELSE DO: /*create it */        
       CREATE RepText.
       ASSIGN   RepText.Brand     = lcBrand 
                RepText.TextType  = 1  
                RepText.LinkCode  = pcId
                RepText.Language  = licount 
                RepText.ToDate    = 12/31/2049
                RepText.FromDate  = TODAY
                RepText.RepText   = lctxt.

      IF llDoEvent THEN RUN StarEventMakeCreateEvent (lhRepText).
   END.



END.

add_struct(response_toplevel_id, "").

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
