/**
 * Get subscription segmentation history 
 *
 * @input string;mandatory;MSISDN
 * @output array;array of segmentation_values;
 * @segmentation_values segmentation_code;string;default SN
                        segmentation_offer;string;default OFF
              segmentation_date;string;DD-MM-YYYY
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
DEF VAR resp_struct AS CHARACTER NO-UNDO. 
DEF VAR resp_array AS CHARACTER NO-UNDO. 
DEF VAR piMsSeq AS INT NO-UNDO. 
DEF VAR liPlaceCode AS INTEGER NO-UNDO. 
DEF VAR liPlaceDate AS INTEGER NO-UNDO. 
DEF VAR liPlaceOffer AS INT NO-UNDO. 
DEF VAR lcDate AS CHARACTER NO-UNDO. 
DEF VAR lcDateFormatted AS CHARACTER NO-UNDO. 
DEF VAR lcSegmentCode AS CHARACTER NO-UNDO. 
DEF VAR lcSegmentOffer AS CHAR NO-UNDO. 
DEF VAR lhmobsub AS HANDLE NO-UNDO.
DEF VAR lhSegment AS HANDLE NO-UNDO.
DEF VAR lcLastDate AS CHAR NO-UNDO.

FUNCTION fResponseRow RETURN LOGICAL
   (INPUT pcSegmentCode AS CHAR,
    INPUT pcDateFormatted AS CHAR,
    INPUT pcSegmentOffer AS CHAR):

   resp_struct = add_struct(resp_array, "").
   add_string(resp_struct, "segmentation_code", pcSegmentCode).
   add_string(resp_struct, "segmentation_date", pcDateFormatted).
   add_string(resp_struct, "segmentation_offer", pcSegmentOffer).
END FUNCTION. 

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.

piMsSeq = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND mobsub where
     mobsub.msseq = piMsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL mobsub then do:
   FIND TermMobsub WHERE
        TermMobsub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL TermMobsub THEN
      return appl_err("Subscription was not found").
   lhmobsub = BUFFER TermMobSub:HANDLE.
END.
ELSE lhmobsub = BUFFER MobSub:HANDLE.

FIND FIRST Segmentation NO-LOCK WHERE
           Segmentation.MsSeq = piMsSeq NO-ERROR.
IF NOT AVAIL Segmentation THEN RETURN appl_err("General").
lhSegment = BUFFER Segment::HANDLE.
resp_array = add_array(response_toplevel_id, "").

FOR EACH Segmentation NO-LOCK WHERE
         Segmentation.MsSeq = piMsSeq:

   lcDateFormatted = string(day(Segmentation.segmentdate),"99") + "-" +
                     string(month(Segmentation.segmentdate),"99") + "-" +
                     string(year(Segmentation.segmentdate),"9999") no-error.

   fResponseRow(Segmentation.SegmentCode,
                lcDateFormatted,
                Segmentation.SegmentOffer).
END.
lcLastDate = lcDateFormatted.
lcSegmentCode = "SN".
lcSegmentOffer = "OFF".
lcDateFormatted = "".

FOR EACH eventlog NO-LOCK where
   eventlog.tablename = "mobsub" and
   eventlog.key = string(lhmobsub::msseq) and
   eventlog.action NE "Delete" use-index tablename
   by eventdate by eventtime:
   
   liPlaceCode = lookup("SegmentCode", eventlog.modifiedfields).
   liPlaceDate = lookup("SegmentDate", eventlog.modifiedfields).
   liPlaceOffer = lookup("SegmentOffer", eventlog.modifiedfields).

   IF liPlaceDate = 0 AND liPlaceCode = 0  AND liPlaceOffer = 0 THEN NEXT.
   
   if liPlaceCode > 0 then 
      lcSegmentCode = entry((liPlaceCode - 1) * 3 + 3, eventlog.datavalues, chr(255)).

   if liPlaceOffer > 0 then 
      lcSegmentOffer = entry((liPlaceOffer - 1) * 3 + 3, eventlog.datavalues, chr(255)).

   if liPlaceDate > 0 then assign 
      lcDate = entry((liPlaceDate - 1) * 3 + 3, eventlog.datavalues, chr(255))
      lcDateFormatted = entry(3,lcDate,"/") + "-" +
                        entry(2,lcDate,"/") + "-" +
                        entry(1,lcDate,"/") no-error.
   
   IF lcLastDate = lcDateFormatted THEN NEXT.
   
   fResponseRow(lcSegmentCode,
                lcDateFormatted,
                lcSegmentOffer).

END.
