/* Subscription Creation Testing Tool */

{xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
katun = "Newton".
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/ftransdir.i}

/* Input parameters */
DEF VAR pcStruct                    AS CHAR NO-UNDO.
DEF VAR pcCustIdType                AS CHAR NO-UNDO.
DEF VAR pcSubsTypes                 AS CHAR NO-UNDO.
DEF VAR pcDataBundleId              AS CHAR NO-UNDO.
DEF VAR pcOtherBundles              AS CHAR NO-UNDO.
DEF VAR pcServices                  AS CHAR NO-UNDO.
DEF VAR pcSegmentCode               AS CHAR NO-UNDO.
DEF VAR piSubsQty                   AS INT  NO-UNDO.
DEF VAR pcUserName                  AS CHAR NO-UNDO.
DEF VAR pcEmailId                   AS CHAR NO-UNDO.

/* Local variables */
DEF VAR lcstruct                    AS CHAR NO-UNDO.
DEF VAR lcCLIType                   AS CHAR NO-UNDO.
DEF VAR lcDel                       AS CHAR NO-UNDO INIT "|".
DEF VAR lcInSpoolDir                AS CHAR NO-UNDO.
DEF VAR lcInIncomingDir             AS CHAR NO-UNDO.
DEF VAR lcInputFile                 AS CHAR NO-UNDO.
DEF VAR liCount                     AS INT  NO-UNDO.

DEF STREAM sInput.

IF validate_request(param_toplevel_id,"string,string,struct") EQ ? THEN RETURN.

ASSIGN pcUserName = get_string(param_toplevel_id,"0")
       pcEmailId  = get_string(param_toplevel_id,"1").

pcStruct = get_struct(param_toplevel_id,"2").
lcstruct = validate_struct(pcStruct,"custid_type,subs_types,data_bundles,other_bundles,service,segment_code,subs_qty").

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcCustIdType = get_string(pcStruct,"custid_type")
   pcSubsTypes  = get_string(pcStruct,"subs_types").

ASSIGN
   pcDataBundleId = get_string(pcStruct,"data_bundles")
                    WHEN LOOKUP("data_bundles",lcstruct) > 0
   pcOtherBundles = get_string(pcStruct,"other_bundles")
                    WHEN LOOKUP("other_bundles",lcstruct) > 0
   pcServices     = get_string(pcStruct,"service")
                    WHEN LOOKUP("service",lcstruct) > 0
   pcSegmentCode  = get_string(pcStruct,"segment_code")
                    WHEN LOOKUP("segment_code",lcstruct) > 0
   piSubsQty      = get_int(pcStruct,"subs_qty")
                    WHEN LOOKUP("subs_qty",lcstruct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pcUserName = "" THEN
   RETURN appl_err("User name is empty").

IF pcEmailId = "" THEN
   RETURN appl_err("Email address is empty").

IF pcCustIdType = "" THEN
   RETURN appl_err("Customer Id Type is empty").

IF pcSubsTypes = "" THEN
   RETURN appl_err("Subscription Type is empty").

IF piSubsQty = ? OR piSubsQty < 0 THEN piSubsQty = 0.

ASSIGN lcInSpoolDir    = fCParam("TestingTool","InSpoolDir")
       lcInIncomingDir = fCParam("TestingTool","InIncomingDir").

IF lcInSpoolDir = "" OR lcInIncomingDir = "" THEN
   RETURN appl_err("TMS Configuration missing").

/* Prepare Subscription creation input file */
lcInputFile = lcInSpoolDir + "/newton_subs_creation_" + pcUserName + "_" + STRING(fMakeTS()) + ".RPT".

OUTPUT STREAM sInput TO VALUE(lcInputFile).

PUT STREAM sInput UNFORMATTED "H|Record_Type|Customer_Type|CLI_Type|Quantity|User_Id|Email_address" SKIP.

DO liCount = 1 TO NUM-ENTRIES(pcSubsTypes).
   lcCLIType = ENTRY(liCount,pcSubsTypes).
   PUT STREAM sInput UNFORMATTED "P" lcDel "SUBSCRIPTION" lcDel
       pcCustIdType lcDel lcCLIType lcDel piSubsQty lcDel
       pcUserName lcDel pcEmailId SKIP.
END. /* DO liCount = 1 TO NUM-ENTRIES(pcSubsTypes). */

IF pcDataBundleId > "" OR pcOtherBundles > "" THEN DO:
   PUT STREAM sInput UNFORMATTED "H|Record_Type|Bundle_LIST" SKIP.
   PUT STREAM sInput UNFORMATTED "P" lcDel "ACT_BONO" lcDel pcDataBundleId SKIP.
   PUT STREAM sInput UNFORMATTED "P" lcDel "ACT_OTHER_BUNDLE" lcDel pcOtherBundles SKIP.
END.

IF pcServices > "" THEN DO:
   PUT STREAM sInput UNFORMATTED "H|Record_Type|Service_List" SKIP.
   PUT STREAM sInput UNFORMATTED "P" lcDel "ACT_SERVICE" lcDel pcServices SKIP.
END.

IF pcSegmentCode > "" THEN DO:
   PUT STREAM sInput UNFORMATTED "H|Record_Type|Segment_Code" SKIP.
   PUT STREAM sInput UNFORMATTED "P" lcDel "SEGMENT_CODE" lcDel pcSegmentCode SKIP.
END.

OUTPUT STREAM sInput CLOSE.

/* Moved input file to incoming directory */
fTransDir(lcInputFile,
          "",
          lcInIncomingDir).

add_boolean(response_toplevel_id,?,True).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.

