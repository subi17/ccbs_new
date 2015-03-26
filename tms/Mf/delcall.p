/* delcall.p

   CHANGES:    07.03.03/aam use fCustCount
*/

{errors.i}
{cdrconst.i}
{fcustcnt.i}

DEF INPUT PARAMETER xrecid AS rec.

DEF VAR xbsub   AS c NO-UNDO.
DEF VAR i-mth   AS i NO-UNDO.
DEF VAR i       AS i NO-UNDO.
DEF VAR x       AS i NO-UNDO.

FIND FIRST FixCDR where 
     recid(FixCDR) = xrecid
NO-LOCK NO-ERROR.

i-mth = (year(FixCDR.Date) * 100) + month(FixCDR.Date).
MCALL:
repeat:
   FIND mthcall where
        mthcall.CustNum = FixCDR.InvCust AND
        mthcall.Month   = i-mth
   EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF fIsErr(errLocked) THEN UNDO MCALL, NEXT MCALL.

   IF AVAIL mthcall THEN 
      ASSIGN mthcall.called = mthcall.called -
             FixCDR.GrossPrice - FixCDR.DiscVal.

   release mthcall.
   LEAVE MCALL.

END.

/* UPDATE accounts IF needed */
FIND FIRST Customer WHERE
           Customer.CustNum = FixCDR.InvCust
NO-LOCK NO-ERROR.
IF AVAILABLE Customer THEN DO:

   FIND FIRST invgroup where 
        invgroup.InvGroup = Customer.InvGroup
   NO-LOCK NO-ERROR.

   IF AVAIL invgroup AND invgroup.UpdCustBal THEN DO:
      fCustCount(Customer.CustNum,
                 "UB",
                 -1 * (FixCDR.GrossPrice  - FixCDR.DiscVal)).
   END. 

END.

/* now DELETE the FixCDR call */
DBL:
repeat:

   FIND CURRENT FixCDR EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF fIsErr(errLocked) THEN UNDO DBL, NEXT DBL.

   DELETE FixCDR NO-ERROR.
   LEAVE DBL.

END.   

