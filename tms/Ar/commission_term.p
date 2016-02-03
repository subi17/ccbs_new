/* ----------------------------------------------------------------------
  MODULE .......: commission_term.p
  TASK .........: Terminate commission
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 03.11.08
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Func/commission.i}

DEF INPUT  PARAMETER iiMsSeq      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icSource     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiTerminated AS INT  NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhFATime AS HANDLE NO-UNDO.
   lhFATime = BUFFER FATime:HANDLE.
   RUN StarEventInitialize(lhFATime).

   DEFINE VARIABLE lhPrePaidRequest AS HANDLE NO-UNDO.
   lhPrePaidRequest = BUFFER PrePaidRequest:HANDLE.
   RUN StarEventInitialize(lhPrePaidRequest).

END.


/***** MAIN start ******/

RUN pTerminateCommission.

fCleanEventObjects().

RETURN RETURN-VALUE.

/****** MAIN end *******/


PROCEDURE pTerminateCommission:

   DEF VAR liCustNum      AS INT NO-UNDO.
   DEF VAR liRelated      AS INT NO-UNDO.
   DEF VAR liFATEndPeriod AS INT NO-UNDO.
   
   DEF VAR liTermReason AS INT NO-UNDO.
   DEF VAR llTerminated AS LOG NO-UNDO.
   DEF VAR llDone       AS LOG NO-UNDO.

   DEF BUFFER bTarg FOR CoTarg.
   
   FOR FIRST MsOwner NO-LOCK WHERE 
             MsOwner.MsSeq = iiMsSeq:
      liCustNum = MsOwner.CustNum.
   END.

   FOR EACH CoTarg NO-LOCK WHERE
            CoTarg.Brand    = gcBrand AND
            CoTarg.TargType = "M"     AND
            CoTarg.CoTarg   = STRING(iiMsSeq):

      ASSIGN
         liFATEndPeriod = 999999
         llDone         = FALSE.
        
      CASE icSource:
         WHEN "termination" THEN 
            liTermReason = (IF COTarg.PromotedID > 0 THEN 5 ELSE 4).
         WHEN "STC" THEN 
            liTermReason = (IF COTarg.PromotedID > 0 THEN 7 ELSE 10).
         WHEN "debt" THEN ASSIGN
            liTermReason   = 0
            liFATEndPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).       
      END.

      /* check for both, fatime and topup */  
      
      RUN pTerminateFAT(CoTarg.CoTargID, 
                        liFATEndPeriod,
                        OUTPUT llTerminated).
      
      IF llTerminated THEN DO:
         llDone = TRUE.
         IF liTermReason > 0 THEN 
            fCommStatus(CoTarg.CoTargID,4,liTermReason,"").
      END.
      
      RUN pTerminateTopUp(CoTarg.CoTargID,
                          liCustNum,
                          OUTPUT llTerminated).
      
      IF llTerminated THEN DO:
         llDone = TRUE.
         IF liTermReason > 0 THEN 
            fCommStatus(CoTarg.CoTargID,4,liTermReason,"").
      END.
      
      /* if subscription is terminated, terminate all that are related to 
         the same order */
      IF icSource = "termination" AND CoTarg.OrderID > 0 THEN 
      FOR EACH bTarg NO-LOCK USE-INDEX OrderID WHERE
               bTarg.Brand   = gcBrand AND
               bTarg.OrderID = CoTarg.OrderID AND
               bTarg.CoTarg NE CoTarg.CoTarg:
         
         liRelated = 0.
         IF bTarg.TargType = "M" THEN 
         FOR FIRST MsOwner NO-LOCK WHERE
                   MsOwner.MsSeq = INTEGER(bTarg.CoTarg):
            liRelated = MsOwner.CustNum.
         END.
         
         RUN pTerminateFAT(bTarg.CoTargID, 
                           liFATEndPeriod,
                           OUTPUT llTerminated).
         
         IF llTerminated THEN DO:
            llDone = TRUE.
            fCommStatus(bTarg.CoTargID,4,liTermReason,"").
         END.
         
         RUN pTerminateTopUp(bTarg.CoTargID,
                             liRelated,
                             OUTPUT llTerminated).
         
         IF llTerminated THEN DO:
            llDone = TRUE.
            fCommStatus(bTarg.CoTargID,4,liTermReason,"").
         END.   
      END.
   
      IF llDone THEN oiTerminated = oiTerminated + 1.
      
   END.      
   
END PROCEDURE.
       

PROCEDURE pTerminateFAT:

   DEF INPUT PARAMETER iiCoTargID AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiToPeriod AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER olTerminated AS LOG NO-UNDO.

   olTerminated = FALSE.
   
   FOR EACH FATime EXCLUSIVE-LOCK USE-INDEX HostTable WHERE
            FATime.Brand     = gcBrand  AND
            FATime.HostTable = "CoTarg" AND
            FATime.KeyValue  = STRING(iiCoTargID) AND
            FATime.InvNum    = 0        AND
            FATime.Period   <= iiToPeriod:

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFATime).

      ASSIGN 
         FATime.InvNum  = 1
         FATime.Used    = FATime.Amt - FATime.TransQty
         FATime.Memo[3] = "CANCELLED".
      
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFATime).
       
      olTerminated = TRUE.

      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "FATime",
                       STRING(FATime.FatNum),
                       FATime.CustNum,
                       "Cancelled",
                       "Commission cancelled due to " + icSource).
   END.

   RETURN "".
   
END PROCEDURE.


PROCEDURE pTerminateTopUp:
 
   DEF INPUT PARAMETER iiCoTargID AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiCustNum  AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olTerminated AS LOG NO-UNDO.

   olTerminated = FALSE.
   
   FOR EACH PrepaidRequest EXCLUSIVE-LOCK USE-INDEX Reference WHERE
            PrepaidRequest.Brand     = gcBrand  AND
            PrepaidRequest.Reference = STRING(iiCoTargID) AND
            PrepaidRequest.Request   = "Commission" AND
            PrepaidRequest.PPStatus  = 0:

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPrepaidRequest).

      PrepaidRequest.PPStatus = 4.
         
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPrepaidRequest).
      
      olTerminated = TRUE.

      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "PrepaidRequest",
                       STRING(PrepaidRequest.PPRequest),
                       iiCustNum,
                       "Cancelled",
                       "Commission cancelled due to " + icSource).
   END.

   RETURN "".
 
END PROCEDURE.



