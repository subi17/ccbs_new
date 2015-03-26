/* Search Subscription based on the mentioned criteria.
 *
 * Parameters are in the toplevel array and in two integers.
 * An array is described here:
 * @input subscription_type         string  - mandatory
          subscription_bundle_id    string  - optional
          data_bundle_id            string  - optional
          other_bundles             string  - optional
          segmentation_code         string  - optional
          payterm                   string  - optional
          term                      string  - optional
          serv_code                 string  - optional
          order_date                date    - optional
          order_status              boolean - optional
 * Integers are described here:
          offset                    integer - mandatory
          limit_of_subscriptions    integer - mandatory
  */
{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
katun = "Newton".
{tmsconst.i}
{fbundle.i}
{fdss.i}

/* Input parameters */
DEF VAR pcCliType      AS CHAR NO-UNDO.
DEF VAR piSubsLimit    AS INT NO-UNDO.
DEF VAR piOffset       AS INT NO-UNDO.
DEF VAR pcSubsBundleId AS CHAR NO-UNDO.
DEF VAR pcDataBundleId AS CHAR NO-UNDO.
DEF VAR pcOtherBundles AS CHAR NO-UNDO.
DEF VAR pcSegmentOffer AS CHAR NO-UNDO.
DEF VAR pcPayTerm      AS CHAR NO-UNDO.
DEF VAR pcTerm         AS CHAR NO-UNDO.
DEF VAR pcBlackBerry   AS CHAR NO-UNDO.
DEF VAR pcStruct       AS CHAR NO-UNDO.
DEF VAR pdtInputDate   AS DATE NO-UNDO. 

/* Local variables */
DEF VAR lcDataBundles  AS CHAR NO-UNDO.
DEF VAR resp_array     AS CHAR NO-UNDO.
DEF VAR top_struct     AS CHAR NO-UNDO.
DEF VAR resp_struct    AS CHAR NO-UNDO.
DEF VAR lcstruct       AS CHAR NO-UNDO.
DEF VAR lcOtherBundle  AS CHAR NO-UNDO.
DEF VAR lcSegmentCode  AS CHAR NO-UNDO.
DEF VAR liCount        AS INT  NO-UNDO.
DEF VAR liNumberOfBundles AS INT  NO-UNDO.
DEF VAR liNumberOfSubs AS INT NO-UNDO     INIT 0.
DEF VAR liNumberLimit  AS INT NO-UNDO     INIT 0.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
DEF VAR ldtOrderDate     AS DATE NO-UNDO. 
DEF VAR liOrderTime      AS INT  NO-UNDO. 
DEF VAR liLoopBegTime    AS INT  NO-UNDO.
DEF VAR liLoopEndTime    AS INT  NO-UNDO. 
DEF VAR liLoopCount      AS INT  NO-UNDO. 
DEF VAR plgOrderStatus   AS LOG  NO-UNDO.  

IF validate_request(param_toplevel_id, "struct,int,int") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
lcstruct = validate_struct(pcStruct,
   "subscription_type!,subscription_bundle_id,data_bundle_id,other_bundles,segmentation_code,payterm,term,serv_code,order_date,order_status").

ASSIGN
   pcCliType      = get_string(pcStruct, "subscription_type")
   piOffset       = get_int(param_toplevel_id, "1")
   piSubsLimit    = get_int(param_toplevel_id, "2")
   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

ASSIGN
   pcSubsBundleId = get_string(pcStruct, "subscription_bundle_id")
      WHEN LOOKUP(pcCliType,lcBundleCLITypes) > 0
   pcDataBundleId = get_string(pcStruct, "data_bundle_id")
      WHEN LOOKUP("data_bundle_id", lcstruct) > 0
   pcOtherBundles = get_string(pcStruct, "other_bundles")
      WHEN LOOKUP("other_bundles", lcstruct) > 0
   pcSegmentOffer = get_string(pcStruct, "segmentation_code")
      WHEN LOOKUP("segmentation_code", lcStruct) > 0
   pcPayTerm      = get_string(pcStruct, "payterm")
      WHEN LOOKUP("payterm", lcStruct) > 0
   pcTerm         = get_string(pcStruct, "term")
      WHEN LOOKUP("term", lcStruct) > 0
   pcBlackBerry   = get_string(pcStruct, "serv_code")
      WHEN LOOKUP("serv_code", lcStruct) > 0
   pdtInputDate   = get_date(pcStruct, "order_date")
      WHEN LOOKUP("order_date", lcStruct) > 0
   plgOrderStatus = get_bool(pcStruct, "order_status")
      WHEN LOOKUP("order_status", lcStruct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pdtInputDate > TODAY THEN 
   pdtInputDate = ?.

liNumberOfBundles = NUM-ENTRIES(pcOtherBundles).

/* Add main array */
ASSIGN top_struct = add_struct(response_toplevel_id, "")
       resp_array = add_array(top_struct, "mobsubs").

liLoopBegTime = TIME.

EACH_MOBSUB:
FOR EACH MobSub NO-LOCK WHERE
         MobSub.Brand   = gcBrand AND
         MobSub.CLIType = pcCliType:
  
   /* If MobSub loop executes more than 30 seconds 
      then it should terminate the execution */
   IF (liLoopCount MOD 100) = 0 THEN DO:
      liLoopEndTime = TIME.

      IF liLoopEndTime - liLoopBegTime >= 30 THEN LEAVE EACH_MOBSUB.

   END.

   liLoopCount = liLoopCount + 1.

   IF pdtInputDate <> ? AND 
      MobSub.TariffActDate >= pdtInputDate THEN NEXT EACH_MOBSUB.
 
   IF pdtInputDate <> ? THEN DO: 
      FOR EACH Order NO-LOCK WHERE 
               Order.MsSeq  EQ MobSub.MsSeq AND
               LOOKUP(Order.StatusCode, {&ORDER_CLOSE_STATUSES}) = 0  
           BY Order.CrStamp DESC:
       
          fSplitTS(Order.CrStamp, 
                   OUTPUT ldtOrderDate,
                   OUTPUT liOrderTime).
          
          IF ldtOrderDate >= pdtInputDate THEN NEXT EACH_MOBSUB.

      END.          
   END.   
   
   IF plgOrderStatus THEN DO: 
      FOR EACH Order NO-LOCK WHERE 
               Order.MsSeq  EQ MobSub.MsSeq                   
           BY Order.CrStamp DESC:
       
         IF NOT Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} THEN
            NEXT EACH_MOBSUB. 
      END.          
   END.   
   
      /* Subscription type */
   IF LOOKUP(pcCliType,lcBundleCLITypes) > 0 AND
      pcSubsBundleId <> MobSub.TariffBundle THEN NEXT EACH_MOBSUB.

      /* Data bundle  */
   IF pcDataBundleId > "" THEN DO:
      lcDataBundles = fGetCurrentSpecificBundle(MobSub.MsSeq,"BONO").
      IF pcDataBundleId <> lcDataBundles THEN NEXT EACH_MOBSUB.
   END. /* IF pcDataBundleId > "" THEN DO: */

      /* Segmentation offer */
   IF pcSegmentOffer > ""  THEN DO:
      FIND FIRST Segmentation NO-LOCK WHERE
                 Segmentation.MsSeq = MobSub.MsSeq NO-ERROR.
      IF NOT AVAIL Segmentation THEN NEXT EACH_MOBSUB.
      IF pcSegmentOffer <> Segmentation.SegmentOffer THEN NEXT EACH_MOBSUB. 
   END.
   
      /* PAYTERMX */
   IF pcPayTerm > "" THEN 
      IF NOT CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                            DCCLI.MsSeq    = MobSub.MsSeq AND
                            DCCLI.DCEvent  = pcPayTerm    AND
                            DCCLI.ValidTo >= TODAY      ) THEN NEXT EACH_MOBSUB.  

      /* TERMX */
   IF pcTerm > "" THEN DO:
      FOR EACH DCCLI NO-LOCK WHERE
               DCCLI.MsSeq      = MobSub.MsSeq AND
               DCCLI.ValidFrom <= TODAY        AND
               DCCLI.ValidTo   >= TODAY        AND
               DCCLI.CreateFees = TRUE         BY DCCLI.ValidFrom DESC:
               
         IF CAN-FIND(FIRST DayCampaign NO-LOCK WHERE
                           DayCampaign.Brand        = gcBrand            AND
                           DayCampaign.DCEvent      = DCCLI.DCEvent      AND
                           DayCampaign.DCEvent      = pcterm             AND
                           DayCampaign.DCType       = {&DCTYPE_DISCOUNT} AND
                           DayCampaign.TermFeeModel NE ""                AND
                           DayCampaign.TermFeeCalc > 0) THEN  
         liCount = liCount + 1.
         
         IF liCount > 1 THEN LEAVE.
      END. /* FOR EACH DCCLI NO-LOCK WHERE */
      IF liCount = 0 THEN NEXT EACH_MOBSUB.
   END.
 
      /* BlackBerry */
   IF pcBlackBerry > "" THEN DO:
      FIND FIRST SubSer WHERE SubSer.ServCom = "BB"          AND
                              SubSer.MsSeq   = MobSub.MsSeq  AND
                              SubSer.SsDate <= TODAY NO-LOCK NO-ERROR.

      IF NOT AVAIL SubSer OR SubSer.SSStat NE 1 THEN NEXT EACH_MOBSUB.
   END.      

/* Other bundles  */
   DO liCount = 1 TO liNumberOfBundles:
      lcOtherBundle = ENTRY(liCount,pcOtherBundles).
      CASE lcOtherBundle:
         WHEN "BONO_VOIP" THEN
            IF fGetActiveSpecificBundle(Mobsub.MsSeq,fMakeTS(),lcOtherBundle) = ""
            THEN NEXT EACH_MOBSUB.
         WHEN "DSS" THEN
            IF NOT fIsDSSActive(MobSub.CustNum,fMakeTS()) THEN NEXT EACH_MOBSUB.
         OTHERWISE NEXT.
      END. /* CASE lcOtherBundle: */
   END. /* DO liCount = 1 TO liNumberOfBundles: */


      /* Count number of subscriptions */
   IF liNumberOfSubs <= piOffset AND piOffset > 0 THEN DO:
      liNumberOfSubs = liNumberOfSubs + 1.
      NEXT EACH_MOBSUB.
   END.
   ELSE DO:
      resp_struct = add_struct(resp_array, "").
      add_string(resp_struct, "cli", MobSub.CLI).
      add_timestamp(resp_struct, "act_stamp", MobSub.ActivationTS).
      liNumberOfSubs = liNumberOfSubs + 1.
      liNumberLimit  = liNumberLimit  + 1.
   END.

   IF liNumberLimit >= piSubsLimit THEN LEAVE EACH_MOBSUB.

END. /* FOR EACH MobSub NO-LOCK WHERE */


FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.



