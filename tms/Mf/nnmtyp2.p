/* -----------------------------------------------
  MODULE .......: NNMTYP2.P
  FUNCTION .....: Maintain monthly Calls 2 (CLOSE ALL)
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 29-12-97
  changePVM ....: 29.05.02/aam nnohti removed, use TMSParam
                  04.11.02/jr  Eventlog
                  07.03.03/aam customer.balance[2] -> CreditLimit
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/tmsparam2.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER Month LIKE MthCall.Month.

DEF VAR InvGroup LIKE InvGroup.InvGroup NO-UNDO.

DEF VAR debt     AS DE NO-UNDO.
DEF VAR i1       AS i  NO-UNDO.
DEF VAR i2       AS i  NO-UNDO.
DEF VAR liClDays AS i  NO-UNDO. 
DEF VAR new_mthcall AS LOG NO-UNDO INIT FALSE.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMthCall AS HANDLE NO-UNDO.
   lhMthCall = BUFFER MthCall:HANDLE.
   RUN StarEventInitialize(lhMthCall).

   DEFINE VARIABLE lhClosedCust AS HANDLE NO-UNDO.
   lhClosedCust = BUFFER ClosedCust:HANDLE.
   RUN StarEventInitialize(lhClosedCust).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMthCall).
   END.
END.

liClDays = fCParamI("ClDays"). 

form
  skip(1)
  "  Instruction: This module adds closing Date to all customers who"   SKIP 
  "               have exceeded their monthly Limit in phone Calls  "   SKIP
  "               within the month given below or have unpaid invoices" SKIP
  "               that are" liClDays "days older than the expiry date" skip(1)
  "               You can also select an invoice group whose data will"  SKIP
  "               be checked."                                          skip(3)
  "               Month ........:" Month     help "Month to handle"       SKIP
  "               Invoice group :" InvGroup 
                     help "Selected invoice group (empty for all)"      skip(4)
WITH
    color value(cfc) title color value(cfc) " Close all customers "
    centered NO-LABEL
    OVERLAY  width 80
    FRAME frm.

cfc = "kline".  RUN Syst/ufcolor.

LOOP:
repeat WITH FRAME frm:

  ehto = 9. RUN Syst/ufkey.
  DISP liClDays WITH FRAME frm.
  UPDATE Month InvGroup WITH FRAME frm.

   toimi:
      repeat WITH FRAME LOOP:
         ASSIGN ufk = 0 ehto = 0 ufk[1] = 7 ufk[5] = 795 ufk[8] = 8.
         RUN Syst/ufkey.
         IF toimi = 1 THEN NEXT  toimi.
         IF toimi = 5 THEN LEAVE toimi.
         IF toimi = 8 THEN LEAVE LOOP.
      END.  /* toimi */


  ASSIGN i1 = 0 i2 = 0.
  message "Checking monthly call counters ...".
  PAUSE 0.
  /* CLOSE ALL who have exceeded their limits */
  FOR EACH MthCall exclusive-lock         where
           MthCall.Month    = Month           AND
           MthCall.Called > MthCall.Limit AND
           MthCall.CloseDate = ?.

      /* IF invoicegroup is selected */
     if InvGroup ne "" THEN DO:

        FIND FIRST Customer where
                   Customer.CustNum = MthCall.CustNum
        no-lock no-error.

        IF NOT AVAIL Customer OR Customer.InvGroup NE InvGroup THEN NEXT.

     END.

     IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMthCall).
     ASSIGN MthCall.CloseDate  = pvm
            MthCall.CloseType = MthCall.CloseType + 1
            i1 = i1 + 1.
     IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMthCall).       

  END.
  HIDE MESSAGE no-pause.
  message "Checking unpaid invoices ...".
  PAUSE 0.
  /* CLOSE ALL who haven't CommPaid their bills in defined time */
  FOR EACH Invoice no-lock where
           Invoice.DueDate <= pvm - liClDays.

     /* IF invoicegroup is selected */
     if InvGroup ne "" THEN DO:

        FIND FIRST Customer where
                   Customer.CustNum = Invoice.CustNum
        no-lock no-error.

        IF NOT AVAIL Customer OR Customer.InvGroup NE InvGroup THEN NEXT.

     END.

     ASSIGN debt = 0.

     FOR EACH Payment of Invoice no-lock.
        debt = debt + Payment.PaymAmt + Payment.Discount.
     END.

     IF Invoice.InvAmt > debt THEN DO:

        /* closing customer .... */
        FIND FIRST MthCall where
                   MthCall.CustNum = Invoice.CustNum AND
                   MthCall.Month     = Month 
        exclusive-lock no-error.

        /* IF already closed .... */
        IF MthCall.CloseDate NE ? THEN NEXT.

        i2 = i2 + 1.


        /* ... IF NOT already there CREATE it */
        IF NOT AVAIL MthCall THEN DO:
           FIND FIRST Customer where
                      Customer.CustNum = Invoice.CustNum 
           no-lock no-error.
           CREATE MthCall.
           ASSIGN MthCall.CustNum = Invoice.CustNum
               MthCall.Month      = Month
               MthCall.Limit      = Customer.CreditLimit
               new_mthcall        = TRUE.
        END.

        IF AVAIL MthCall THEN 
        DO:
           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMthCall).
           new_mthcall = false.
        END.

        /* ... IF NOT Printed into wl -file change closing Date */
        IF MthCall.Printed = FALSE THEN ASSIGN MthCall.CloseDate = pvm.

        /* ... IF NOT already marked AS unpaid invoices */
        IF MthCall.CloseType <= 1 THEN ASSIGN
           MthCall.CloseType = MthCall.CloseType + 2.

        IF new_mthcall AND
           llDoEvent THEN RUN StarEventMakeCreateEvent(lhMthCall).

        IF NOT new_mthcall AND
           llDoEvent THEN RUN StarEventMakeModifyEvent(lhMthCall).

        new_mthcall = FALSE.

        /* UPDATE closed customers -file IF needed */
        FIND FIRST ClosedCust where
                   ClosedCust.CustNum = MthCall.CustNum AND
                   ClosedCust.Printed = FALSE 
        exclusive-lock no-error.
        IF AVAIL ClosedCust AND
           llDoEvent THEN RUN StarEventSetOldBuffer(lhClosedCust).

        IF AVAIL ClosedCust AND ClosedCust.Called <= 1 THEN
           ASSIGN ClosedCust.Called = ClosedCust.Called + 2.

        IF AVAIL ClosedCust AND
           llDoEvent THEN RUN StarEventMakeModifyEvent(lhClosedCust).
     END.

  END.

  MESSAGE string(i1) +
       " customers were closed because of exceeded limit".
  MESSAGE string(i2) +
       " customers were closed because of unpaid invoices - press ENTER !".
  PAUSE no-message.

  LEAVE LOOP.

END. /* LOOP */
HIDE FRAME frm no-pause.

