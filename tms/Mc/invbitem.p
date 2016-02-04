/* ----------------------------------------------------------------------
  MODULE .......: invbitem.p
  TASK .........: Show ALL OTHER Billed items of a invoice
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 27.03.2001
  CHANGED ......: 15.09.2003/aam brand
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}

DEF INPUT PARAMETER  InvNum     AS I  NO-UNDO.
DEF INPUT PARAMETER  BillCode      AS C  NO-UNDO.

DEF VAR  Qty        AS I  NO-UNDO.

ASSIGN
   ufk = 0 ehto = 3. RUN Syst/ufkey.

   FIND FIRST SingleFee WHERE 
              SingleFee.InvNum  = InvNum AND
              SingleFee.BillCode = BillCode NO-LOCK.

   IF NOT AVAIL SingleFee THEN DO:
      MESSAGE "This invoice " InvNum " has NO O.B.I record !"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.

   PAUSE 0.
   FOR
   EACH  SingleFee NO-LOCK WHERE
         SingleFee.InvNum = InvNum  AND
         SingleFee.BillCode = BillCode
         BREAK BY SingleFee.Memo[1].

      FIND BillItem WHERE 
           BillItem.Brand    = gcBrand AND
           BillItem.BillCode = SingleFee.BillCode NO-LOCK no-ERROR.

      DISP
      SingleFee.CustNum
      SUBSTRING(SingleFee.Memo[1],7) column-label "A-Number"
      SingleFee.BillPeriod
      SingleFee.Concerns[1]
      SingleFee.Amt
      BillItem.BIName  format "x(25)" WHEN AVAIL BillItem

      WITH CENTERED OVERLAY ROW 3 11 DOWN
      TITLE " ALL BILLING ITEMS (OBI) ON INVOICE " + STRING(SingleFee.InvNum)  
      FRAME SUB.


      IF FRAME-LINE(sub) < FRAME-DOWN(sub) THEN DOWN.
      ELSE DO:
         PAUSE 0.
         MESSAGE "MORE OTHER Billed ITEMS: PRESS ENTER !".
         PAUSE NO-MESSAGE.
      END.   


   END.
   PAUSE 0.
   MESSAGE "PRESS ENTER TO CONTINUE !".
   PAUSE NO-MESSAGE.

   HIDE FRAME sub NO-PAUSE.

