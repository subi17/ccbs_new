/* -----------------------------------------------
  MODULE .......: NNIGCL.P
  FUNCTION .....: CLOSE invoicegroups
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 29-12-97
  MODIFIED .....: 04.11.02 jr Eventlog
                  07.03.2003/aam customer.balance[2] -> CreditLimit
                  15.09.2003/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER Month LIKE MthCall.Month.

DEF VAR i1 AS i NO-UNDO.
DEF VAR InvGroup LIKE InvGroup.InvGroup NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMthCall AS HANDLE NO-UNDO.
   lhMthCall = BUFFER MthCall:HANDLE.
   RUN StarEventInitialize(lhMthCall).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhMthCall).
   END.
END.

form
  skip(1)
  "  Instruction: This module adds closing Date to all customers who"  SKIP
  "               belong to the INVOICE GROUP given below.          "  skip(1)
  "               If a customer has not made any Calls his monthly  "  SKIP
  "               call row will be created with a zero value.       "  skip(3)
  "               Invoice group :" InvGroup   
                  help "Invoice group to close"    skip(7)
WITH
    color value(cfc) title color value(cfc) " CLOSE AN INVOICE GROUP " 
    centered NO-LABEL
    OVERLAY  width 80
    FRAME frm.

cfc = "kline".  RUN ufcolor.

LOOP:
repeat WITH FRAME frm:

  ehto = 9. RUN ufkey.
  UPDATE InvGroup WITH FRAME frm.

   toimi:
      repeat WITH FRAME LOOP:
         ASSIGN ufk = 0 ehto = 0 ufk[1] = 7 ufk[5] = 795 ufk[8] = 8.
         RUN ufkey.
         IF toimi = 1 THEN NEXT  toimi.
         IF toimi = 5 THEN LEAVE toimi.
         IF toimi = 8 THEN LEAVE LOOP.
      END.  /* toimi */


  ASSIGN i1 = 0.
  /* CLOSE ALL customers from an invoice group */
  FOR EACH Customer no-lock where
           Customer.Brand    = gcBrand AND
           Customer.InvGroup = InvGroup:

     FIND FIRST MthCall where 
                MthCall.CustNum = Customer.CustNum
     exclusive-lock no-error.


     IF NOT AVAIL MthCall THEN DO:
        CREATE MthCall.
        ASSIGN
           MthCall.CustNum = Customer.CustNum
           MthCall.Month     = Month
           MthCall.Limit   = Customer.CreditLimit
           MthCall.CloseDate  = pvm i1 = i1 + 1.
        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMthCall).   
     END.
     ELSE
     DO:
        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMthCall).
        ASSIGN MthCall.CloseDate  = pvm i1 = i1 + 1.
        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMthCall).
     END.
  END.


  LEAVE LOOP.

END. /* LOOP */

message i1  "customers closed of invoice group" InvGroup " - hit ENTER !".
PAUSE no-message.

HIDE FRAME frm no-pause.


