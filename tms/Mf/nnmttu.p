/* -----------------------------------------------
  MODULE .......: nnmttu.p
  FUNCTION .....: Report of monthly Calls
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 09-11-97
  MODIFIED .....: 18-12-97 kl => added All/Closed customers
                                 show also negative debt values
                  29.05.02 aam nnohti removed, 
                               calculate debt from a/r AccNum
                  11.09.02 jp Invgroup validation             
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/utumaa.i "new"}

assign tuni1 = "nnmttu"
       tuni2 = "".

DEF STREAM tul.

DEF VAR rl      AS i    NO-UNDO init 9999.
DEF VAR sl      AS i    NO-UNDO.
DEF VAR ok      AS lo   NO-UNDO.
DEF VAR i       AS i    NO-UNDO.
def var b-all   as lo   no-undo format "All/Closed" init TRUE.
def var b-exc   as lo   no-undo format "All/Exceeded" init TRUE.
def var Month     as i    no-undo format "999999".
DEF VAR debt    AS i    NO-UNDO.
DEF VAR CommPaid    AS i    NO-UNDO.
DEF VAR due     AS i    NO-UNDO.
DEF VAR oldest  AS i    NO-UNDO.
DEF VAR CloseDate  AS DA   NO-UNDO.
def var s-head  as c    no-undo format "x(40)".

DEF VAR lev      AS i NO-UNDO init 112.
DEF VAR InvGroup  LIKE InvGroup.InvGroup NO-UNDO.
DEF VAR IGName  LIKE InvGroup.IGName NO-UNDO.

form header /* tulosteen pAAotsikko */
   fill ("=",lev) format "x(112)" SKIP
   ynimi AT 1
   "MONTHLY CALLS"   at 33  s-head  at 50 pvm format "99-99-99" TO 112 SKIP
   "month" at 33 Month at 39  IGName at 50 "Page"  TO 107
      sl format "ZZZ9" TO 112 SKIP
   fill ("=",lev) format "x(112)" skip(1)

   "Cust"       TO 10
   "Customer's" AT 12
   "Total"      TO 52
   "Monthly"    TO 64
   "Closed"     TO 74

   "---- Customer's unpaid bills ----"   TO 112
                SKIP
   "number"     TO 10
   "name"       AT 12
   "calls"      TO 52
   "limit"      TO 64
   "on"         TO 74
   "Debt"       TO 87
   "Due"        TO 98
   "Days"       TO 108

   fill ("-",lev) format "x(112)" SKIP

WITH
   width 112 NO-LABEL no-box
   FRAME sivuots.

form
    skip(1)
"  Instruction:  This program prints out a listing of either all   "    skip
"                or only those customers who have been suspended   "    skip
"                within the month given below.                     "    skip(1)
"                Report will be sorted by customer number.         "
skip(3)

"                Month ..........:" Month
                 help "Month to be listed (YYYYMM)"                      SKIP
"                Invoice group ..:" InvGroup
                 help "Invoice group / empty for all"                    SKIP
"                Customers"                                              skip
"                 -closed / all .:" b-all
                 help "All/those customers who have been closed (A/C) "  SKIP
"                 -limit exceeded:" b-exc
                 help "Customers with exceeded Limit / all (A/E)"        skip(3)
WITH
    COLOR value(cfc) TITLE COLOR value(cfc)
    " " + ynimi + " Monthly call counter report " + string(pvm,"99-99-99") + " "
    ROW 1 width 80 NO-LABEL
    FRAME rajat.

Month = (year(pvm) * 100) + month(pvm).

rajat:
repeat WITH FRAME rajat:

   ehto = 9. RUN Syst/ufkey.p.
   UPDATE Month 
   InvGroup VALIDATE(CAN-FIND(invgroup WHERE
                              invgroup.invgroup = INPUT invgroup),
   "Unknown Invoice Group!")                                        
    b-all b-exc WITH FRAME rajat.
   if InvGroup ne "" THEN DO:
      FIND FIRST InvGroup where InvGroup.InvGroup = InvGroup no-lock no-error.
      if avail InvGroup then assign IGName = "-" + InvGroup.IGName.
      ELSE DO:
         BELL.
         message "Invoice group does not exist !!". PAUSE 2.
         NEXT-PROMPT InvGroup.
         NEXT rajat.
      END.
   END.
   else assign IGName = "-all invoice groups".

toimi:
   repeat WITH FRAME rajat:
      ASSIGN
      ufk = 0 ehto = 0
      ufk[1] = 7 ufk[5] = 63 ufk[8] = 8.
      RUN Syst/ufkey.p.
      IF toimi = 1 THEN NEXT  rajat.
      IF toimi = 8 THEN LEAVE rajat.
      IF toimi = 5 THEN LEAVE toimi.
  END.  /* toimi */

   /* set header FOR report */
   if b-all     then s-head = "-all customers".
   else              s-head = "-closed customers".
   if not b-exc then s-head = s-head + " with exceeded limit".

   tila = TRUE.
   {Syst/tmsreport.i "leave rajat"}

   message "Printing ...".
   FOR EACH MthCall no-lock  where
            MthCall.Limit > 0     AND
           (IF NOT b-all   THEN                  /* closed OR ALL */
            MthCall.CloseDate  NE ?
            ELSE TRUE)            AND
           (IF NOT b-exc   THEN
            MthCall.Called > MthCall.Limit       /* Limit exceeded OR ALL */
            ELSE TRUE)            AND
           MthCall.Month     =  Month,

     FIRST Customer no-lock  where
           Customer.CustNum  = MthCall.CustNum AND
          (if InvGroup ne "" THEN                 /* one invoice group OR ALL */
           Customer.InvGroup = InvGroup
           else Customer.InvGroup ne "")

   BREAK
      BY   MthCall.CustNum.

      ASSIGN debt = 0 oldest = 0 i = i + 1.

      /* check unpaid invoices */
      FOR EACH Invoice where
               Invoice.CustNum  = MthCall.CustNum AND
               Invoice.PaymState  < 2
               no-lock.

         /* check ALL payments */
         debt = Invoice.InvAmt.
         FOR EACH Payment of Invoice no-lock.
            DO i = 1 TO 10.
               IF Payment.AccType[i] = 1 
               THEN debt = debt + Payment.Posting[i].
            END.
         END.

         /* calculate due */
         IF TODAY > Invoice.DueDate THEN
              ASSIGN due    = debt
                     oldest = TODAY - Invoice.DueDate.
         ELSE ASSIGN due    = 0
                     oldest = 0.

         accum due    (sub-total).
         accum oldest (MAXIMUM).

      END.

      IF last-of(MthCall.CustNum) THEN DO:
         /* every MthCall record will be Printed out */
         IF rl >= skayt1 THEN DO:
            PUT STREAM tul UNFORMATTED skip(spit1 - rl).
            sl = sl + 1.
            view STREAM tul FRAME sivuots.
            rl = 7. /* rlx = 0 */.
         END.

         PUT STREAM tul UNFORMATTED
            MthCall.CustNum              TO 10
            Customer.CustName              AT 12
            MthCall.Called               to 52  format "zzz,zz9"
            MthCall.Limit                to 64  format "zzz,zz9"
            MthCall.CloseDate               to 74  format "99-99-99"
            debt                         to 86  format "-zzz,zz9"
            (accum sub-total due)        to 97  format "-zzz,zz9".

         IF oldest NE 0 THEN PUT STREAM tul UNFORMATTED
            accum maximum   oldest          to 108 format "zz9".
         ELSE PUT STREAM tul UNFORMATTED 0  TO 108.

         PUT STREAM tul SKIP.
         rl = rl + 1.
      END. /* last-of */

   END. /* FOR EACH MthCall */

   PUT STREAM tul UNFORMATTED skip(spit1 - rl).
   ASSIGN tila = FALSE.
   {Syst/tmsreport.i}

   IF i = 0 THEN DO:
      BELL.
      message "No counters found with given criteria ! -press ENTER ".
      PAUSE no-message.
   END.
   LEAVE.

END. /* rajat */
HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.

