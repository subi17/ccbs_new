 /* ------------------------------------------------------
  MODULE .......: camprunco.p
  FUNCTION .....: collect customers for campaign run
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 18.02.04
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Mc/camprundf.i}
{Func/timestamp.i}

DEF INPUT  PARAMETER iiCustNum1    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiCustNum2    AS INT  NO-UNDO.
DEF INPUT  PARAMETER idtDate1      AS DATE NO-UNDO.
DEF INPUT  PARAMETER idtDate2      AS DATE NO-UNDO. 
DEF OUTPUT PARAMETER TABLE FOR ttCust.

DEF VAR liCount AS INT  NO-UNDO.
DEF VAR liPer1  AS INT  NO-UNDO.
DEF VAR liPer2  AS INT  NO-UNDO.

ASSIGN liPer1 = fMake2Dt(idtDate1,0)
       liPer2 = fMake2Dt(idtDate2,86399).

EMPTY TEMP-TABLE ttCust.

FORM
liCount AT 2 LABEL "CLIs found" 
WITH SIDE-LABELS OVERLAY ROW 15 CENTERED TITLE " Collecting "
FRAME fQty1.

FOR EACH Customer NO-LOCK WHERE
         Customer.Brand    = gcBrand    AND
         Customer.CustNum >= iiCustNum1 AND
         Customer.CustNum <= iiCustNum2,
    EACH MobSub OF Customer NO-LOCK WHERE
         MobSub.ActivationDate >= idtDate1 AND
         MobSub.ActivationDate <= idtDate2:

   CREATE ttCust.
   ASSIGN ttCust.CustNum = MobSub.CustNum
          ttCust.MSSeq   = MobSub.MSSeq
          liCount        = liCount + 1.
           
   IF liCount < 100 OR liCount MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liCount WITH FRAME fQty1.
   END.

END.

FOR EACH Customer NO-LOCK WHERE
         Customer.Brand    = gcBrand    AND
         Customer.CustNum >= iiCustNum1 AND
         Customer.CustNum <= iiCustNum2,
    EACH MsOwner OF Customer NO-LOCK WHERE
         MsOwner.TsBeg >= liPer1 AND
         MsOwner.TsBeg <= liPer2:

   IF CAN-FIND(FIRST ttCust WHERE
                     ttCust.CustNum = MsOwner.CustNum AND
                     ttCust.MSSeq   = MSOwner.MsSeq)
   THEN NEXT. 
   
   CREATE ttCust.
   ASSIGN ttCust.CustNum = MsOwner.CustNum
          ttCust.MSSeq   = MsOwner.MSSeq
          liCount        = liCount + 1.

   IF liCount < 100 OR liCount MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liCount WITH FRAME fQty1.
   END.

END.


HIDE FRAME fQty1 NO-PAUSE. 

