/**
 * Get invoice groups
 *
 * @input id;array of int;mandatory;invoices groups ids
 * @output ;array of struct of InvoiceTargetGroup
 * @InvoicetargetGroup id; int;InvoicetargetGroup ID
                       default;boolean;if it is the default group or not
                       active;boolean;if it is active or not 
                       fusion;boolean;if it is fusion group
                       subscriptions;array of Invoicetargets of this InvoiceTargetGroup
 * @ subscriptions id;int; Invoicetarget id
                   msisdn;string; CLI of the subscription
                   terminated;boolean; if subscription has been termianted 
                   valid_from;date;
                   valid_to;date;
 * @
           
*/

{newton/src/header_get.i}
{Func/timestamp.i}
{Syst/tmsconst.i}

DEF VAR piId AS INT NO-UNDO.
DEF VAR lcInvoiceTargets AS CHAR NO-UNDO.
DEF VAR lcInvoiceTarget AS CHAR No-UNDO.
DEF VAR llTerminated AS LOG NO-UNDO.
DEF VAR lcCLI AS CHAR NO-UNDO.
DEF VAR lcHistories AS CHAR NO-UNDO.
DEF VAR liPlaceDate AS INT NO-UNDO.
DEF VAR liPlaceDefault AS INT NO-UNDO.
DEF VAR lcDate AS CHAR NO-UNDO.
DEF VAR lcNewDateFormatted AS CHAR NO-UNDO.
DEF VAR lcOldDateFormatted AS CHAR NO-UNDO. 

FUNCTION fAddHistory RETURNS LOGICAL 
      (INPUT pcHistoriesArray AS CHAR,
       INPUT pdEventDate AS DATE,
       INPUT pcEventTime AS CHAR,
       INPUT pcDescription AS CHAR):

       DEF VAR lcHistory AS CHAR NO-UNDO.
       DEF VAR ldTS AS DEC NO-UNDO.

       ldTS =  fHMS2TS(pdEventdate,pcEventTime).
       lcHistory = add_struct(pcHistoriesArray,"").
       add_timestamp(lcHistory,"time",ldTS).
       add_string(lcHistory,"description",pcDescription).

       RETURN TRUE.
END FUNCTION.

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   piId = get_int(pcIDArray, STRING(liCounter)).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   FIND InvoiceTargetGroup WHERE InvoiceTargetGroup.ITGroupID = piId NO-LOCK NO-ERROR. 
   IF NOT AVAIL InvoiceTargetGroup THEN 
       RETURN appl_err("InvoiceTargetGroup not found").
   
   lcResultStruct = add_struct(resp_array, "").
   add_int(lcResultStruct, "id", InvoiceTargetGroup.ITGroupID). 
   add_boolean(lcResultStruct,"default",InvoiceTargetGroup.DefaultGroup).
   add_boolean(lcResultStruct,"active",(IF InvoiceTargetGroup.ToDate > TODAY THEN TRUE ELSE FALSE)).

   IF InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL} OR
      InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} THEN
      add_boolean(lcResultStruct,"fusion",TRUE).

   /* add InvoiceTargets */
   lcInvoiceTargets = add_array(lcResultStruct,"subscriptions").
   FOR EACH InvoiceTarget NO-LOCK WHERE
            InvoiceTarget.ITGroupID = InvoiceTargetGroup.ITGroupID :
            lcInvoiceTarget = add_struct(lcInvoiceTargets,"").
            add_int(lcInvoiceTarget,"msseq",InvoiceTarget.MsSeq).
            
            llTerminated = FALSE. 
            lcCLI = "".
            FIND MobSub WHERE
                 MobSub.MsSeq = InvoiceTarget.MsSeq NO-LOCK NO-ERROR.
            IF NOT AVAIL MobSub THEN DO:
               FIND TermMobSub WHERE
                    TermMobSub.MSSeq = InvoiceTarget.MsSeq NO-LOCK NO-ERROR.
               IF AVAIL TermMobSub THEN
                    ASSIGN llTerminated = TRUE
                           lcCLI =TermMobSub.CLI.
            END.
            ELSE DO:
               lcCLI = MobSub.CLI .
               IF MobSub.PayType THEN llTerminated = TRUE.
            END.

            add_string(lcInvoiceTarget,"msisdn",lcCLI).
            add_boolean(lcInvoiceTarget,"terminated",llTerminated).
            add_datetime(lcInvoiceTarget, "valid_from", InvoiceTarget.FromDate).
            IF InvoiceTarget.ToDate <= TODAY THEN 
                add_datetime(lcInvoiceTarget, "valid_to", InvoiceTarget.ToDate).      
   END.

   /* add history */
   lcHistories = add_array(lcResultStruct,"history").
   FOR EACH EventLog NO-LOCK WHERE
            EventLog.TableName = "InvoiceTargetGroup" AND
            EventLog.Key = STRING(InvoiceTargetGroup.ITGroupID) USE-INDEX TableName
   BY EventDate BY EventTime:
  
     IF EventLog.Action EQ "Create" THEN DO:
        fAddHistory(lcHistories,
                    EventLog.EventDate,
                    EventLog.EventTime,
                    "Group was activated").
        NEXT.
     END.
     
     IF EventLog.Action NE "Modify" THEN NEXT. 
     
     liPlaceDate  = LOOKUP("ToDate", EventLog.ModifiedFields).
     liPlaceDefault = LOOKUP("DefaultGroup", EventLog.ModifiedFields).
     IF liPlaceDate = 0 AND liPlaceDefault = 0 THEN NEXT.
     
     IF liPlaceDefault > 0 AND
        LOGICAL(ENTRY((liPlaceDefault - 1) * 3 + 3, EventLog.DataValues, CHR(255))) THEN
        fAddHistory(lcHistories,
                    EventLog.EventDate,
                    EventLog.EventTime,
                    "Group was set to default").

     IF liPlaceDate > 0 THEN DO: 
        lcDate = ENTRY((liPlaceDate - 1) * 3 + 3, EventLog.DataValues, CHR(255)).
        lcNewDateFormatted = ENTRY(3,lcDate,"/") + "-" +
                             ENTRY(2,lcDate,"/") + "-" +
                             ENTRY(1,lcDate,"/") NO-ERROR.
        lcDate = ENTRY((liPlaceDate - 1) * 3 + 2, EventLog.DataValues, CHR(255)).
        lcOldDateFormatted = ENTRY(3,lcDate,"/") + "-" +
                             ENTRY(2,lcDate,"/") + "-" +
                             ENTRY(1,lcDate,"/") NO-ERROR.     
        fAddHistory(lcHistories,
                    EventLog.EventDate,
                    EventLog.EventTime,
                    (IF DATE(lcOldDateFormatted) > DATE(lcNewDateFormatted) THEN "Group was deactivated" 
                     ELSE "Group was activated")).
     END.

  END.



END.
