/* ---------------------------------------------------------------------------
  MODULE .......: IROWSFEE
  FUNCTION .....: Browse billable items of a single fee
  APPLICATION ..: TMS
  AUTHOR .......: aam (from nnlrcoit)
  CREATED ......: 14-02-03
  MODIFIED .....: 15.09.03/aam brand
                  05.03.04/aam layout changes to frame lis (lcBelongs etc.)
                  19.03.04 aam input CLI, use temp-table,
                               renamed nnlrbit -> irowsfee
                  30.03.04 kl  fixes after index changes
                  31.05.04 aam CalcObj
                  16.08.05 aam CLIType
                  20.11.06 aam new db structure (ordercustomer)
  VERSION ......: M15
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER iiInvNum     AS I    NO-UNDO.
DEF INPUT PARAMETER icBillCode   AS c    NO-UNDO.
DEF INPUT PARAMETER icCLI        AS CHAR NO-UNDO. 

def /* new */ shared var siirto as char.

def var BillPeriod      like SingleFee.BillPeriod  no-undo.
def var lcBIName      like BillItem.BIName     no-undo.
def var Amt        like SingleFee.Amt    no-undo.
def var xrecid         as recid                        init ?.
def var firstline      as int                 no-undo  init 0.
def var order          as int                 no-undo  init 1.
def var ordercount     as int                 no-undo  init 1.
def var ufkey          as log                 no-undo  init true.
def var delline        as int                 no-undo  init 0.
def var ex-order       as int                 no-undo.
def var memory         as recid               no-undo.
def var line           as int format "99"     no-undo.
def var must-print     as log                 no-undo.
def var must-add       as log                 no-undo.
def var fr-header      as char                no-undo.
def var rtab           as recid extent 24     no-undo.
def var i              as int                 no-undo.
def var rc             as int                 no-undo.
def var ok             as log format "Yes/No" no-undo.

DEF VAR lcBelongs   AS CHAR NO-UNDO.
DEF VAR lcOtherData AS CHAR NO-UNDO. 
DEF VAR ldPer       AS DEC  NO-UNDO EXTENT 2.
DEF VAR lcCLIType   AS CHAR NO-UNDO. 


DEF TEMP-TABLE ttRow NO-UNDO
   FIELD SingleFee AS INT
   FIELD BillPer   AS INT
   FIELD CLIType   AS CHAR
   INDEX BillPer BillPer.


IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   RUN StarEventInitialize(lhSingleFee).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhSingleFee).
   END.
END.

form
    SingleFee.BillPeriod
    SingleFee.Concerns[1] column-label "Fr.Per." format "99999999"
    SingleFee.Concerns[2] column-label "To Per." format "99999999"
    ttRow.CLIType         COLUMN-LABEL "CLIType" FORMAT "X(8)"
    SingleFee.Amt         column-label "Amt"     
    SingleFee.Memo[1]     column-label "Memo"    format "x(26)"
with
    centered row 2 overlay scroll 1 13 down
    color value(cfc) title color value(ctc) " " +
    substr(Customer.CustName,1,18) + " / " + substr(lcBIName,1,18) +
    ": Invoice = " + string(iiInvNum) + ", CustNo = " +
    string(Customer.CustNum) + " "  frame sel.

form
    "CustomerNo .:" SingleFee.CustNum                       SKIP
    "Billing Item:" SingleFee.BillCode                      SKIP 
    "Period .....:" SingleFee.BillPeriod                    skip
    "Amount .....:" SingleFee.Amt                           skip
    "Belongs to .:" lcBelongs FORMAT "X(40)"                SKIP
    "Calc Object :" SingleFee.CalcObj                       SKIP
    "Invoice ....:" SingleFee.InvNum                        SKIP
    "-------------"                                         SKIP
    SingleFee.Memo[1]                                       SKIP
    SingleFee.Memo[2]                                       SKIP
    lcOtherData FORMAT "X(50)"                              SKIP

 with  overlay row 5 centered
    color value(cfc) title color value(ctc) fr-header with no-label
    frame lis.

form /*  search with field CustNum */
    BillPeriod
    help "Give Period YyyyMm"
    with row 4 col 2 title color value(ctc) " FIND PERIOD "
    color value(cfc) no-labels overlay frame f1.

form /* memo */
with
    overlay row 7 centered no-label
    color value(cfc) title color value(cfc) " Memo "
    frame memo.


FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK.   
FIND Customer OF Invoice NO-LOCK no-error.

FOR EACH SingleFee NO-LOCK WHERE
         SingleFee.InvNum   = iiInvNum           AND
         SingleFee.BillCode = icBillCode:
         
   ok = FALSE.
      
   IF SingleFee.HostTable = "MobSub" THEN DO:
   
      ASSIGN ldPer[1] = SingleFee.Concerns[1]
             ldPer[2] = SingleFee.Concerns[2].
      IF ldPer[1] = 0 THEN ldPer[1] = SingleFee.BillPer * 100 + 1. 
      IF ldPer[2] = 0 THEN ldPer[2] = ldPer[1].
             
      FOR EACH MSOwner NO-LOCK WHERE
               MsOwner.MSSeq   = INTEGER(SingleFee.KeyValue) AND
               MsOwner.TsBeg  <= ldPer[2]                    AND
               MsOwner.TsEnd  >= ldPer[1]
      BY MsOwner.TsEnd DESC:         
         
         IF icCli = "" OR MsOwner.CLI = icCLI 
         THEN DO:
            ASSIGN ok        = TRUE
                   lcCLIType = MsOwner.CLIType.
            LEAVE.
         END.
      END.
   END.
      
   IF (icCLI > "" AND NOT ok) OR
      (icCLI = "" AND ok)
   THEN NEXT. 
         
   CREATE ttRow.
   ASSIGN ttRow.SingleFee = RECID(SingleFee)
          ttRow.BillPer   = SingleFee.BillPeriod
          ttRow.CLIType   = lcCLIType.
END.

FIND FIRST ttRow NO-ERROR. 

if available ttRow then assign
   memory     = recid(ttRow)
   must-print = true
   must-add   = false.
else do:
   bell. message
   "ERROR: NO billable items at all  - press ENTER !".
   pause no-message.
   return.
end.

cfc = "sel". RUN Syst/ufcolor. assign ccc = cfc.
view frame sel.
LOOP:
repeat with frame sel:

    if order <> ex-order then do:
       ex-order = order.
    end.
print-line:
   do:
      if must-print then do:
        up frame-line - 1.
        find ttRow where recid(ttRow) = memory    no-lock no-error.
        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* if a line has just been deleted, then ... */
        if delline > 0 then down delline - 1.

        repeat with frame sel:
           if available ttRow then do:

              RUN local-disp-row.
              rtab[frame-line] = recid(ttRow).
              if order = 1 then find next ttRow no-error.
           end.
           else do:
              clear no-pause.
              rtab[frame-line] = ?.
           end.
           if frame-line = frame-down then leave.
           down.
        end.
        up frame-line - 1.
        down firstline.
        assign firstline = 0
               must-print = false.
        pause 0 no-message.

        /* one page of data has been printed and
        the cursor is in the upmost line for 'choose' */
      end. /* must-print = true */
   end. /* print-line */

   /* if lastly a line has been deleted */
   if delline > 0 then down delline - 1.
   assign delline = 0.

BROWSE:
   repeat with frame sel on endkey undo, return:

      if ufkey then do:
        assign
        ufk[1]= 771 ufk[2]= 0  ufk[3]= 927 ufk[4]= 0
        ufk[5]= 0   ufk[6]= 0  ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
        ehto  = 3 ufkey = false.
        RUN Syst/ufkey.p.
      end.

      hide message no-pause.
      if order = 1 then do:
        choose row SingleFee.BillPeriod ;(uchoose.i;) no-error with frame sel.
        color display value(ccc) SingleFee.BillPeriod with frame sel.
      end.

      if rtab[frame-line] = ? then next.

      nap = keylabel(lastkey).

      if lookup(nap,"cursor-right") > 0 then do:
        order = order + 1. if order > ordercount then order = 1.
      end.
      if lookup(nap,"cursor-left") > 0 then do:
        order = order - 1. if order = 0 then order = ordercount.
      end.

      if order <> ex-order then do:
        assign firstline = 0 memory = rtab[frame-line].
        find ttRow where recid(ttRow) = memory.
        do i = 1 to frame-line - 1:
           if order = 1 then find prev ttRow no-error.
           if available ttRow then
              assign firstline = i memory = recid(ttRow).
           else leave.
        end.
        must-print = true.
        next LOOP.
      end.

      if rtab[frame-line] = ? and not must-add then do:
        bell.
        message "You are on a empty row, move upwards !".
        pause 1 no-message.
        next.
      end.

      assign nap = keylabel(lastkey).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 then do with frame sel:
        if frame-line = 1 then do:
           find ttRow where recid(ttRow) = rtab[1].
           if order = 1 then find prev ttRow no-error.
           if not available ttRow then do:
              message "YOU ARE ON THE FIRST ROW !".
              bell. pause 1 no-message.
              next BROWSE.
           end.
           else do:
              /* a previous one was found */
              scroll down.

              RUN local-disp-row.

              do i = frame-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              end.
              assign
              rtab[1] = recid(ttRow)
              memory = rtab[1].
           end.
        end.
        else up 1.
      end. /* previous line */

      /* next line */
      else if lookup(nap,"cursor-down") > 0 then do
      with frame sel:
        if frame-line = frame-down then do:
           find ttRow where recid(ttRow) = rtab[frame-down].
           if order = 1 then find next ttRow no-error.
           if not available ttRow then do:
              message "YOU ARE ON THE LAST ROW !".
              bell. pause 1 no-message.
              next BROWSE.
           end.
           else do:
              /* yet another record was found */
              scroll up.

              RUN local-disp-row.

              do i = 1 to frame-down - 1:
                 rtab[i] = rtab[i + 1].
              end.
              rtab[frame-down] = recid(ttRow).
              /* finally last line's keyvalue is saved */
              memory = rtab[1].
           end.
        end.
        else down 1 .
      end. /* next line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 then do:
        memory = rtab[1].
        find ttRow where recid(ttRow) = memory no-error.
        if order = 1 then find prev ttRow no-error.
        if available ttRow then do:
           memory = recid(ttRow).

           /* go back one page */
           do line = 1 to (frame-down - 1):
              if order = 1 then find prev ttRow no-error.
              if available ttRow then memory = recid(ttRow).
              else line = frame-down.
           end.
           must-print = true.
           next LOOP.
        end.
        else do:
           /* this is the first data page */
           message "YOU ARE ON THE FIRST PAGE !".
           bell. pause 1 no-message.
        end.
     end. /* previous page */

     /* next page */
     else if lookup(nap,"next-page,page-down,+") > 0 then do with frame sel:
       /* cursor to the downmost line */
       if rtab[frame-down] = ? then do:
           message "YOU ARE ON THE LAST PAGE !".
           bell. pause 1 no-message.
       end.
       else do: /* the downmost line wasn't empty */
           memory = rtab[frame-down].
           find ttRow where recid(ttRow) = memory .
           must-print = true.
           next LOOP.
       end.
     end. /* next page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 then do on endkey undo, next LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       BillPeriod = 0.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       update BillPeriod with frame f1.
       hide frame f1 no-pause.
       if BillPeriod <> ? then do:
          find first ttRow where
                     ttRow.BillPer >= BillPeriod no-error.
          if not available ttRow then do:
             bell.
             message "NONE FOUND !".
             pause 1 no-message.
             next BROWSE.
          end.
          /*  ttRow was found */
          assign order = 1 memory = recid(ttRow) must-print = true.
          next LOOP.
       end.
     end. /* Haku sar. 1 */


     if lookup(nap,"3,f3") > 0 then     /* memo */
     do trans with frame memo on endkey undo, next LOOP:
       assign ehto = 9 cfc = "lis" ufkey = true.
       RUN Syst/ufkey. RUN Syst/ufcolor.
       find ttRow where recid(ttRow) = rtab[frame-line(sel)].
       FIND SingleFee WHERE RECID(SingleFee) = ttRow.SingleFee NO-LOCK.

       /*
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).
       */
       display SingleFee.Memo [1 for 5] with frame memo 1 col.

       PAUSE MESSAGE "Press ENTER to continue".

       hide frame memo no-pause.
       pause 0.
       disp SingleFee.Memo[1] with frame sel.
     end.

     else if lookup(nap,"5,f5") > 0 and ufk[5] > 0 then do:  /* lisays */
        must-add = true.
        next LOOP.
     end.

     else if lookup(nap,"enter,return") > 0 then
     do with frame lis transaction on endkey undo, next LOOP:
       /* change */

       assign fr-header = " VIEW " cfc = "lis".  RUN Syst/ufcolor.

       find ttRow where recid(ttRow) = rtab[frame-line(sel)].
       FIND SingleFee WHERE RECID(SingleFee) = ttRow.SingleFee NO-LOCK.

       lcOtherData = "".
       /* get more details from order */
       IF SingleFee.HostTable = "order" THEN
       FOR FIRST Order NO-LOCK WHERE
                 Order.Brand   = SingleFee.Brand AND
                 Order.OrderID = INTEGER(SingleFee.KeyValue):
                 
          lcOtherData = "CLI: " + Order.CLI.
          
          
          FOR FIRST OrderCustomer NO-LOCK WHERE
                    OrderCustomer.Brand   = gcBrand AND
                    OrderCustomer.OrderID = Order.OrderID AND
                    OrderCustomer.RowType = Order.UserRole:

              lcOtherData = lcOtherData + " User: " + 
                            DYNAMIC-FUNCTION("fDispOrderName" IN ghFunc1,
                                             BUFFER OrderCustomer) +
                           (IF OrderCustomer.BirthDay NE ?
                            THEN STRING(OrderCustomer.BirthDay,"99.99.9999")
                            ELSE "").
          END.
       END. 

       display 
       SingleFee.BillCode
       SingleFee.CustNum
       SingleFee.BillPeriod
       SingleFee.Amt
       SingleFee.InvNum
       SingleFee.HostTable + " " + SingleFee.KeyValue @ lcBelongs
       SingleFee.CalcObj
       SingleFee.Memo[1] 
       SingleFee.Memo[2]
       lcOtherData
       with frame lis.
       xrecid = recid(ttRow).

       PAUSE 0.
       MESSAGE "PRESS ENTER TO CONTINUE !".
       PAUSE NO-MESSAGE.
     end.


     else if lookup(nap,"home,h") > 0 then do:
       if order = 1 then find first ttRow no-error.
       assign memory = recid(ttRow) must-print = true.
       next LOOP.
     end.

     else if lookup(nap,"end,e") > 0 then do : /* last record */
       if order = 1 then find last ttRow no-error.
       assign memory = recid(ttRow) must-print = true.
       next LOOP.
     end.

     else if lookup(nap,"8,f8") > 0 then leave LOOP.

  end.  /* BROWSE */
end.  /* LOOP */

hide frame sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

   FIND SingleFee WHERE RECID(SingleFee) = ttRow.SingleFee NO-LOCK.

   find BillItem where 
        BillItem.Brand    = gcBrand AND
        BillItem.BillCode = SingleFee.BillCode no-lock no-error.
   if avail BillItem then lcBIName = BillItem.BIName.
   else lcBIName = "!! UNKNOWN !!!".

   display SingleFee.Memo[1]
           SingleFee.Amt 
           SingleFee.BillPeriod 
           SingleFee.Concerns[1] FORMAT "99999999"
           SingleFee.Concerns[2] FORMAT "99999999"                 
           ttRow.CLIType
           Memo[1]
           WITH FRAME sel.

END PROCEDURE.

