/* ------------------------------------------------------------------------
  MODULE .......: nnsuse.p
  FUNCTION .....: Yhden laskun suoritusten BROWSE
  APPLICATION ..: VP
  AUTHOR .......: TT
  CREATED ......: 23.10.1997
  CHANGED ......: 13.10.1998 pt
                  12.11.2002 jr New Memo
                  12.11.2002 jr Check meme record instead memo extents
                  15.09.2003/aam brand
                  06.02.04   jp custnum for memo
  Version ......: M15
------------------------------------------------------------------------ */

{Syst/commali.i}

DEF INPUT  PARAMETER InvNum  LIKE Payment.InvNum  NO-UNDO.
DEF OUTPUT PARAMETER Voucher LIKE Payment.Voucher NO-UNDO.

DEF VAR dtos   LIKE Payment.Voucher NO-UNDO EXTENT 10.
DEF VAR rtab AS RECID NO-UNDO EXTENT 10.

form
   Payment.Voucher  column-label "Voucher"
   help "Choose payment !"   
   Payment.PaymDate   column-label "Paid"
   Payment.AccDate    column-label "Registered"
   Payment.AccNum[1]  column-label "Acct"
   Payment.PaymAmt    column-label "Money"
   Payment.Discount   column-label "Discnt"
WITH
  10 DOWN centered ROW 5 OVERLAY
  title " PAYMENTS ON INVOICE " + string(InvNum) FRAME suo.

voucher = 0.

IF NOT can-find(FIRST Payment where Payment.InvNum = InvNum) THEN DO:
   bell. message "No previous payments on invoice no." InvNum "!".
   RETURN.
END.

DO WITH FRAME suo:

   FOR EACH Payment no-lock where Payment.InvNum = InvNum WITH FRAME suo.

      DISP
      Payment.Voucher
      Payment.PaymDate
      Payment.AccDate
      AccNum [1]
      Payment.PaymAmt
      Payment.Discount 
      CAN-FIND(FIRST memo WHERE
                     memo.Brand     = Payment.Brand AND
                     memo.HostTable = "Payment"     AND
                     memo.KeyValue  = STRING(Payment.Voucher) AND
                     memo.memotext NE "") format "Notes!/"
      .

      dtos[FRAME-LINE] = Payment.Voucher.
      rtab[FRAME-LINE] = recid(Payment).

      IF FRAME-LINE < FRAME-DOWN THEN DOWN.
      ELSE LEAVE.
   END.
   up FRAME-LINE - 1.

   repeat WITH FRAME suo:
      ASSIGN ufk = 0 ehto = 3 ufk[1] = 992 ufk[5] = 11 ufk[8] = 8.
      RUN ufkey.

      CHOOSE ROW Payment.Voucher {Syst/uchoose.i} no-error.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      if lookup(keylabel(lastkey),"1,f1") > 0 THEN DO:
         RUN memo(INPUT 0,
                  INPUT "Payment",
                  INPUT STRING(dtos[FRAME-LINE]),
                  INPUT "Payment").
         NEXT.
      END.

      else if lookup(keylabel(lastkey),"enter,return,5,f5") > 0 THEN DO:
         Voucher = dtos[FRAME-LINE].
         LEAVE.
      END. 

      else if lookup(keylabel(lastkey),"8,f8") > 0 THEN LEAVE.

      BELL.
   END.
END.
HIDE FRAME suo.

