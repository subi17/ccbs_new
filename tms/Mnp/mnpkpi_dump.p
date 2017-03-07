/* ----------------------------------------------------------------------
MODULE .......: mnpkpi_dump.p
TASK .........: MNP IN/OUT KPI dump for the Track. SER-465, YOT-1223
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 04.03.11
Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Syst/dumpfile_run.i}

DEF INPUT PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.


DEF VAR llErrorLog AS LOGICAL NO-UNDO INIT FALSE.
DEF VAR liTime AS INTEGER NO-UNDO. 
DEF VAR ldeNWFrom AS DECIMAL NO-UNDO. 

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

IF llErrorLog THEN DO:
   DEF STREAM serr.
   OUTPUT STREAM serr TO VALUE("/tmp/mnpkpi_errors.txt") APPEND.
END.


/* counters */
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcMNPKPI AS CHARACTER NO-UNDO. 

def stream sout.
output stream sout to value(icFile).

DEFINE VARIABLE lcCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCreated AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcUpdated AS CHARACTER NO-UNDO.

FUNCTION fTS2ModifiedHMS RETURNS CHARACTER
   (INPUT ideTimeStamp AS DECIMAL):
   DEFINE VARIABLE dte AS DATE NO-UNDO.
   DEFINE VARIABLE tme AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcStamp AS CHARACTER NO-UNDO. 

   fSplitTS(ideTimeStamp, OUTPUT dte, OUTPUT tme).
   lcStamp = STRING(TRUNCATE(ideTimeStamp,0)).
   lcStamp = STRING(lcStamp,"9999-99-99") + " " + STRING(tme,"HH:MM:SS").
   RETURN lcStamp.
END FUNCTION.


   FOR EACH mnpprocess where
            mnpprocess.brand = gcBrand AND
            mnpprocess.mnptype = {&MNP_TYPE_IN} and
            mnpprocess.updatets > idLastDump NO-LOCK:

      run pMNPINKPI(mnpprocess.mnpseq, output lcMNPKPI).
      ASSIGN
         lcCreated = fTS2ModifiedHMS(mnpprocess.createdts)      
         lcUpdated = fTS2ModifiedHMS(mnpprocess.updatets).     
      /* Convert TMS status to character value */
      FIND TMSCodes WHERE 
           TMSCodes.TableName = "MNPProcess" AND
           TMSCodes.FieldName = "StatusCode" AND
           TMSCodes.CodeGroup = "MNP" AND
           TMSCodes.CodeValue = STRING(MNPProcess.StatusCode) NO-LOCK NO-ERROR.
      IF AVAIL TMSCodes THEN
         lcCode = (IF TMSCodes.CodeName = "AREC_CLOSED" THEN "AREC"
                 ELSE TMSCodes.CodeName).
      ELSE lcCode = STRING(MNPProcess.StatusCode).

      find first mnpsub where
                 mnpsub.mnpseq = MNPProcess.mnpseq NO-LOCK.

      put stream sout unformatted
         mnpprocess.mnpseq "|"
         mnpprocess.mnptype "|"
         mnpprocess.orderid "|"
         mnpsub.cli "|"
         lcCreated "|"
         lcUpdated "|"
         lcCode "|"
         mnpprocess.opercode "|"
         lcMNPKPI skip.

      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents WITH FRAME fColl.
      END.

   END.

   FOR EACH mnpprocess where
            mnpprocess.brand = gcBrand AND
            mnpprocess.mnptype = {&MNP_TYPE_OUT} and
            mnpprocess.updatets > idLastDump NO-LOCK:

      RUN pMNPOutKPI(mnpprocess.mnpseq, output lcMNPKPI).

      ASSIGN
         lcCreated = fTS2ModifiedHMS(mnpprocess.createdts)      
         lcUpdated = fTS2ModifiedHMS(mnpprocess.updatets).     
      /* Convert TMS status to character value */
      FIND TMSCodes WHERE 
           TMSCodes.TableName = "MNPProcess" AND
           TMSCodes.FieldName = "StatusCode" AND
           TMSCodes.CodeGroup = "MNP" AND
           TMSCodes.CodeValue = STRING(MNPProcess.StatusCode) NO-LOCK NO-ERROR.
      IF AVAIL TMSCodes THEN
         lcCode = (IF TMSCodes.CodeName = "AREC_CLOSED" THEN "AREC"
                   ELSE TMSCodes.CodeName).
      ELSE lcCode = STRING(MNPProcess.StatusCode).

      FOR EACH mnpsub where
               mnpsub.mnpseq = mnpprocess.mnpseq NO-LOCK:
         put stream sout unformatted
            mnpprocess.mnpseq "|"
            mnpprocess.mnptype "|"
            mnpsub.msseq "|"
            mnpsub.cli "|"
            lcCreated "|"
            lcUpdated "|"
            lccode "|"
            mnpprocess.opercode "|"
            lcMNPKPI skip.
      END.
      
      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents WITH FRAME fColl.
      END.

   END.

IF NOT SESSION:BATCH THEN HIDE FRAME fColl NO-PAUSE.

PROCEDURE pMNPINKPI:

   DEFINE INPUT PARAMETER piMNPSeq AS INTEGER NO-UNDO. 
   DEFINE OUTPUT PARAMETER ocResult AS CHARACTER NO-UNDO.

   DEFINE VARIABLE ldeTime AS DEC NO-UNDO format "99999999.99999".
   DEFINE VARIABLE ldaActDate AS DATE NO-UNDO. 
   DEFINE VARIABLE ldaActTS AS DEC NO-UNDO format "99999999.99999".
   DEFINE VARIABLE lrmnpoperation AS ROWID NO-UNDO.
   DEFINE VARIABLE liProcesses AS INTEGER NO-UNDO. 

   DEF BUFFER bMNPProcess for mnpprocess.
   DEF BUFFER bmp FOR mnpprocess.
   DEF BUFFER mnpprocess_orig FOR mnpprocess.
   DEF BUFFER MNPProcess FOR mnpprocess.

   release mnpprocess_orig.

   find mnpprocess where
        mnpprocess.mnpseq = piMNPSeq NO-LOCK.
   
   find order where
        order.brand = gcBrand and
        order.orderid = mnpprocess.orderid NO-LOCK.

   /* check order status */
   IF LOOKUP(order.statuscode,{&ORDER_INACTIVE_STATUSES}) = 0 THEN DO:
      ocResult = "".
      RETURN.
   END.
   
   find mnpprocess where
        mnpprocess.order = order.orderid and
        mnpprocess.mnptype = 1 NO-LOCK NO-ERROR.
   
   /* not unique mnpprocess or mnpprocess does not exist */
   IF NOT AVAIL mnpprocess THEN DO:

      /* count total number of processes */
      FOR EACH bMNPProcess where
         bMNPProcess.orderid = order.orderid and
         bMNPProcess.mnptype = 1 NO-LOCK:
         liProcesses = liProcesses + 1.
      END.

      /* find first mnpprocess based on creation time */
      FOR first bmp WHERE
         bmp.orderid = order.orderid NO-LOCK by bmp.createdts:
         find mnpprocess_orig where
           rowid(mnpprocess_orig) = rowid(bmp) NO-LOCK.
      end.
      
      /* find latest mnpprocess 
         (it could be that active process is not the latest one) */
      find first mnpprocess where
                 mnpprocess.order = order.orderid and
         lookup(string(mnpprocess.statuscode),"5,6") > 0 NO-LOCK NO-ERROR.
         
      if not avail mnpprocess then do:
         FOR LAST bmp WHERE
            bmp.orderid = order.orderid NO-LOCK by bmp.createdts:
            find mnpprocess where
              rowid(mnpprocess) = rowid(bmp) NO-LOCK.
         end.
      end.

   END.
   ELSE liProcesses = 1.

   /* fetch the first created process for checking the original creation date */
   IF NOT AVAIL mnpprocess_orig then do:
      find mnpprocess_orig where
         recid(mnpprocess_orig) = recid(mnpprocess) NO-LOCK.
   end.

   /* order.mnpstatus is not in sync with mnpprocess.statuscode */
   if ((order.mnpstatus ne mnpprocess.statuscode + 1) and not 
       (order.mnpstatus eq 5 and mnpprocess.statuscode eq 8)) then do:

      if order.statuscode = "6" then 
           ocResult = "ORDER_DELIVERED_ANALYSE_FAILED".
      else ocResult = "ORDER_CLOSED_ANALYSE_FAILED".
      RETURN.
   end.

   /* ACAN and closed order */
   if mnpprocess.statuscode = 7 then do:

      if lookup(order.statuscode,{&ORDER_CLOSE_STATUSES}) = 0 then do:
         IF llErrorLog THEN put stream serr order.orderid " cancelled mnp, order should be closed" skip.
          ocResult = "ORDER_CLOSED_ANALYSE_FAILED".
      end.
      else ocResult = "CLOSED_CANCELLED (" + mnpprocess.statusreason + ")".
      RETURN.
   end.

   /* AREC and closed order */
   if mnpprocess.statuscode = 4 or 
      mnpprocess.statuscode = 8 then do:
      
      if lookup(order.statuscode,{&ORDER_CLOSE_STATUSES}) > 0 then do:
         ocResult = "REJECTED (" +
                    mnpprocess.statusreason + ")".
      end.
      else do: /* shouldn't be possible */
         IF llErrorLog THEN put stream serr order.orderid 
            " rejected mnp, but order is delivered" skip.
         ocResult = "ORDER_DELIVERED_ANALYSE_FAILED".
      end.
      RETURN.
   end.
   
   /* order is closed but not AREC or ACAN */
   IF lookup(order.statuscode,{&ORDER_CLOSE_STATUSES}) > 0 THEN DO:
      
      if order.msseq ne 0 then do:
         
         FIND mobsub WHERE
              mobsub.msseq = order.msseq NO-LOCK NO-ERROR.
                  
         IF AVAIL mobsub then do:
            IF llErrorLog THEN put stream serr order.orderid
              " cancelled order, but subscription exists" skip.
         end.
      end.
      ocResult = "ORDER_CLOSED_ANALYSE_FAILED".
      RETURN.
   END.

   /* order is delivered, but check if it is activated in time*/
   if mnpprocess.statuscode ne 6 and
      mnpprocess.statuscode ne 5 then do:
         IF llErrorLog THEN put stream serr order.orderid 
         " incorrect mnpprocess status " mnpprocess.statuscode skip.
      ocResult = "ORDER_DELIVERED_ANALYSE_FAILED".
      RETURN.
   END.

   /* find subscription network activation time */
   FIND mobsub WHERE
        mobsub.msseq = order.msseq NO-LOCK NO-ERROR.

   IF NOT AVAIL mobsub then do:
      
      FIND termmobsub WHERE
           termmobsub.msseq = order.msseq NO-LOCK NO-ERROR.

      IF NOT AVAIL termmobsub THEN DO:
         IF llErrorLog THEN PUT STREAM serr unformatted 
            Order.OrderID 
            " delivered order, but subscription not found" SKIP.
         ocResult = "ORDER_DELIVERED_ANALYSE_FAILED".
         RETURN.
      END.

      ldaActDate = termmobsub.activationdate.
      ldaActTS = termmobsub.activationts.
   END.

   ELSE ASSIGN
      ldaActTS = mobsub.activationts
      ldaActDate = mobsub.activationdate.
   
   /* if only one mnpprocess exists use porting time instead of CW date */
   IF liProcesses = 1 THEN DO:
      ldeTime = mnpprocess.portingtime.
   END.
   /* multiple mnp processes, get original CW date */
   ELSE IF mnpprocess_orig.portingtime > 0 then do:
       ldeTime = mnpprocess_orig.portingtime.
   END.

   /* mnpprocess is delayed, check the reason of delay */
   IF ldaActTS - ldeTime  > 0.216 THEN DO:  /* more than 6 hours late */
      
      if mnpprocess_orig.statuscode = 0 then do:
            
         lrmnpoperation = ?.
         FOR FIRST mnpoperation WHERE
                   mnpoperation.mnpseq = mnpprocess_orig.mnpseq AND
                   mnpoperation.messagetype = 
                      "crearSolicitudIndividualAltaPortabilidadMovil"
            NO-LOCK BY sentts:
            lrmnpoperation = ROWID(mnpoperation). 
         END.

         FIND mnpoperation WHERE 
            ROWID(mnpoperation) = lrmnpoperation NO-LOCK NO-ERROR.
         
         if not avail mnpoperation then do:
            ocResult = "DELIVERED_DELAYED_OTHER".
            RETURN.
         end.
         else do: 
            ocResult = "ORDER_DELIVERED_DELAYED_REJECTED (" + 
                       mnpoperation.errorcode + ")".
            RETURN.
         END.
      end.
      /* mnp delayed, original mnp process was rejected */
      else if mnpprocess_orig.statuscode = 4 OR
              mnpprocess_orig.statuscode = 8 
         then do:
         ocResult = "ORDER_DELIVERED_DELAYED_REJECTED (" +
                    mnpprocess_orig.statusreason + ")".
         RETURN.
      end.
      else if mnpprocess_orig.statuscode = 6 or
              mnpprocess_orig.statuscode = 5 then do:
         find msrequest where msrequest.msseq = order.msseq and
          msrequest.reqtype = 13 NO-LOCK.
          if index(msrequest.memo, "error") > 0 then 
            ocResult = "DELIVERED_DELAYED_NW".
          else do:
            find solog where
                 solog.msseq = msrequest.msseq and
                 solog.msrequest = msrequest.msrequest NO-LOCK NO-ERROR.
            IF AVAIL solog then do:
               IF solog.timeslottms > solog.createdts
               THEN ldeNWFrom = solog.timeslottms.
               ELSE ldeNWFrom = solog.createdts.
               
               /* 4 hour delay */
               IF (ldeNWFrom > 0 and solog.completedts > 0) and
                  solog.completedts - ldeNWFrom > 0.144 THEN
                  ocResult = "DELIVERED_DELAYED_NWC".
               ELSE ocResult = "DELIVERED_DELAYED_OTHER".
            end.
            else ocResult = "DELIVERED_DELAYED_OTHER".
          end.
          RETURN.
      end.
      else do:
         IF llErrorLog THEN PUT STREAM serr unformatted Order.OrderID 
            " delivered delayed order, unknown delay reason" SKIP.
          ocResult = "DELIVERED_DELAYED_OTHER".
          RETURN.
      end.
      RETURN.
   end.
   ocResult = "DELIVERED_OK".
END.
      
PROCEDURE pMNPOutKPI:
   
   DEFINE INPUT PARAMETER piMNPSeq AS INTEGER NO-UNDO. 
   DEFINE OUTPUT PARAMETER ocResult AS CHARACTER NO-UNDO.

   DEFINE VARIABLE ldaActTS AS DEC NO-UNDO format "99999999.99999".
   DEF BUFFER MNPProcess FOR mnpprocess.
   ldaActTS = 0.

   find mnpprocess where
        mnpprocess.mnpseq = piMNPSeq NO-LOCK.
      
   /* cancelled */
   if mnpprocess.statuscode = 7 then do:
     ocResult = "CLOSED_CANCELLED (" + mnpprocess.statusreason + ")".
     RETURN.
   end.

   /* rejected */
   if mnpprocess.statuscode = 4 then do:
      ocResult = "REJECTED (" + mnpprocess.statusreason + ")".
      return.
   end.

   /* delivered */
   if lookup(string(mnpprocess.statuscode),"6") > 0 then do:
            
      if mnpprocess.updatets - mnpprocess.portingtime <= 0.216 THEN do:
         ocResult = "DELIVERED_OK".
         RETURN.
      END.
      
      FOR EACH mnpsub where
               mnpsub.mnpseq = mnpprocess.mnpseq NO-LOCK:

         /* use 10 minute buffer because BB service termination
            can move subscription termination time onwards */
         find msrequest no-lock where
              msrequest.msseq = mnpsub.msseq and
              msrequest.reqtype = 18 and
              msrequest.actstamp >= mnpprocess.portingtime and
              msrequest.actstamp < fsecoffset(mnpprocess.portingtime, 600) 
         use-index msseq no-error.
         
         IF AVAIL msrequest then do:

            if msrequest.reqstatus ne 2 then do:
               IF llErrorLog THEN put stream serr mnpprocess.portrequest 
                  "|delivered, termination request status "
                  msrequest.reqstatus skip.
               ocResult  = "DELIVERED_DELAYED_OTHER".
               return.
            end.

            /* more than 6 hours late */
            else if msrequest.donestamp - msrequest.actstamp > 0.216 THEN do:
               if index(msrequest.memo, "error") > 0
                  then ocResult = "DELIVERED_DELAYED_NW".
               else do:
                 find solog where
                      solog.msseq = msrequest.msseq and
                      solog.msrequest = msrequest.msrequest NO-LOCK NO-ERROR.
                 IF AVAIL solog then do:
                    IF solog.timeslottms > solog.createdts
                    THEN ldeNWFrom = solog.timeslottms.
                    ELSE ldeNWFrom = solog.createdts.
                    
                    /* 4 hour delay */
                    IF (ldeNWFrom > 0 and solog.completedts > 0) and
                       solog.completedts - ldeNWFrom > 0.144 THEN
                       ocResult = "DELIVERED_DELAYED_NWC".
                    ELSE ocResult = "DELIVERED_DELAYED_OTHER".
                 end.
                 else ocResult = "DELIVERED_DELAYED_OTHER".
               end.
               return.
            end.
            else do:
               ocResult = "DELIVERED_OK".
            end.

         end.
         else do:

            find first msrequest where
                       msrequest.msseq = mnpsub.msseq and
                       msrequest.reqtype = 18 and
                       msrequest.reqstatus = 2 NO-LOCK use-index msseq.
            
            if not avail msrequest then do:
               IF llErrorLog THEN put stream serr mnpprocess.portrequest 
                  "|terminated, termination request not found" skip.
               ocResult = "DELIVERED_DELAYED_OTHER".
               return.
            end.
            else if msrequest.actstamp <= mnpprocess.portingtime then do:
               ocResult = "DELIVERED_OK".
            end.
            else do:
               IF llErrorLog THEN put stream serr unformatted
                  msrequest.cli " " msrequest.msseq               
               " termination request timestamp does not match with porting time"                skip.
               ocResult = "DELIVERED_DELAYED_OTHER".
               return.
            end.
            
         end.
            
      end.
      RETURN.

   end.  
   else ocResult = "".

END PROCEDURE. 
