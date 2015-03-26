/*------------------------------------------------------
  Module .......: searchcust.p
  FUNCTION .....: Choose customer
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 26.02.04
  MODIFIED .....: 20.04.06/aam icCriteria may contain additional data ("|")
                  22.11.06/aam icCriteria may contain id type  
  Version ......: TF
  ------------------------------------------------------ */

{commali.i}
{fcustdata.i}

DEF INPUT  PARAMETER icCriteria AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icValue    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ilDisp     AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER oiCustNum  AS INT  NO-UNDO.

define temp-table ttCustomer
  FIELD CustNum   like customer.custnum
  FIELD CustName  like customer.custname
  FIELD OrgID     LIKE Customer.OrgID
  index CustNum is primary CustNum.

def var custnum     as int                  no-undo.
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.
def var reply       as ch                   no-undo.
DEF VAR lctitle     AS CHAR                 NO-UNDO.
DEF VAR lcAddCrit   AS CHAR                 NO-UNDO.
DEF VAR lcIDType    AS CHAR                 NO-UNDO.

DEF BUFFER bCust FOR Customer.

IF ilDisp THEN DO:
form
    ttCustomer.CustNum 
    ttCustomer.CustName
    ttCustomer.OrgID 
    with scroll 1 5 down  row 11 centered 
    title lctitle   overlay frame sel.

   cfc = "sel". run ufcolor. assign ccc = cfc.
END.

DEF BUFFER xxCustomer FOR Customer.

CASE ENTRY(1,icCriteria,"|"):
   WHEN "ORGID"     THEN lctitle = "CUSTOMERS (Customer ID) "   + icValue.
   WHEN "AGREEMENT" THEN lcTitle = "CUSTOMERS (Agreement) "     + icvalue.
   WHEN "USERCUST"  THEN lcTitle = "CUSTOMERS (User) OF "       + icValue.
   WHEN "INVCUST"   THEN lcTitle = "CUSTOMERS (Invoice Cust.) OF " + icvalue.
END CASE.

IF icCriteria BEGINS "ORGID" OR 
   icCriteria BEGINS "AGREEMENT" THEN DO:
   
   ASSIGN lcAddCrit = ""
          lcIDType  = "".
          
   IF NUM-ENTRIES(icCriteria,"|") > 1 THEN DO:

       lcAddCrit = ENTRY(2,icCriteria,"|").
       
       IF NUM-ENTRIES(icCriteria,"|") > 2 THEN 
          lcIDType = ENTRY(3,icCriteria,"|").
   END.
    
   for each customer no-lock where 
            customer.brand = gcbrand and
            customer.orgid = icValue and
            Customer.Roles NE "inactive":

       CASE lcAddCrit:
       WHEN "SELF" THEN DO:
          IF Customer.CustNum NE Customer.InvCust OR
             Customer.AgrCust NE Customer.CustNum
          THEN NEXT.
       END.
       END CASE.

       IF lcIDType > "" AND Customer.CustIDType NE lcIDType THEN NEXT.
            
       create ttCustomer.
       assign
          ttCustomer.CustNum  = Customer.Custnum.
          ttCustomer.OrgID    = Customer.OrgID.
          ttCustomer.CustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                 BUFFER Customer).
   END.                         
end.

ELSE IF  icCriteria BEGINS "USERCUST" THEN 

for each Customer no-lock where 
         Customer.InvCust = INT(icValue) AND 
         Customer.Roles NE "inactive":

      create ttCustomer.
      assign
         ttCustomer.Custnum  = Customer.CustNum
         ttCustomer.OrgID    = Customer.OrgID.
      
      ASSIGN
         ttCustomer.CustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                               BUFFER Customer).
END.

ELSE IF icCriteria BEGINS "InvCust" THEN 
for each customer no-lock where 
         customer.AgrCust =  INT(icValue) AND
         Customer.Roles NE "inactive":

   IF NUM-ENTRIES(icCriteria,"|") > 1 THEN DO:

       lcAddCrit = ENTRY(2,icCriteria,"|").
       
       CASE lcAddCrit:
       WHEN "SELF" THEN DO:
          IF Customer.CustNum NE Customer.InvCust THEN NEXT.
       END.
       OTHERWISE DO:
          IF NOT CAN-FIND(FIRST bCust WHERE
                                bCust.InvCust = Customer.CustNum AND
                                bCust.CustNum = INTEGER(lcAddCrit)) THEN NEXT.
       END.
       END CASE.
   END.
   
   create ttCustomer.
   assign
      ttCustomer.CustNum  = Customer.Custnum
      ttCustomer.OrgID    = Customer.OrgID.

   ASSIGN
      ttCustomer.CustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                             BUFFER Customer).
                                     
end.

If ILDisp = FALSE THEN DO:
   find first ttCustomer no-lock no-error.
   IF AVAIL ttCustomer THEN oiCustnum = ttCustomer.Custnum.
   RETURN.
END.


MAIN:
repeat:

   find first ttCustomer  no-lock no-error.

   if not available ttCustomer then do:
      oiCustNum = 0.
      leave main.
   end.
   else do:
      If ILDisp = FALSE THEN DO:
         oiCustnum = ttCustomer.Custnum.
         LEAVE main.
      END.
      memory = recid(ttCustomer).
      must-print = true.
   end.

pause 0.
view frame  sel.

LOOP:
   Repeat with frame sel:

print-line:
   do :
      if must-print then do:
         clear frame sel all no-pause.
         find ttCustomer where recid(ttCustomer) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available ttCustomer:
            display
            ttCustomer.CustNum
            ttCustomer.CustName
            ttCustomer.OrgID
            with frame sel.
            rtab[frame-line] = recid(ttCustomer).
            down with frame sel.
            find next ttCustomer no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 
         ufk[4] = 5
         ufk[5] = 11 ufk[8] = 8  ufk[9] = 1
         ehto = 3 ufkey = false.
         run ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row ttCustomer.CustNum ;(uchoose.i;) no-error with frame sel.
         color display value(ccc) ttCustomer.CustNum with frame sel.

         nap = keylabel(lastkey).

         if nap = "8" or nap = "f8" then leave MAIN. /* Return */

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find ttCustomer where recid(ttCustomer) = rtab[frame-line] no-lock.
               find prev ttCustomer no-lock no-error.
               if not available ttCustomer then do:
                  bell.
                  message "You are on 1st row !".              
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* a previous one was found */
                  scroll down.
                  do i = 11 to 2 by -1:
                     rtab[i] = rtab[i - 1].
                  end.
                  display ttCustomer.CustNum
                          ttCustomer.CustName
                          ttCustomer.OrgID.
                  rtab[frame-line] = recid(ttCustomer).
                  memory = recid(ttCustomer).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
            find ttCustomer where recid(ttCustomer) = rtab[frame-line] no-lock .
               find next ttCustomer  WHERE no-lock no-error.
               if not available ttCustomer then do:
                  bell.
                  message "You are on last row !".
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* yet another record was found */
                  scroll up.
                  do i = 1 to 10:
                     rtab[i] = rtab[i + 1].
                  end.
                  display 
                     ttCustomer.CustNum
                     ttCustomer.CustName
                     ttCustomer.OrgID.
                  rtab[frame-line] = recid(ttCustomer).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find ttCustomer where recid(ttCustomer) = memory no-lock no-error.
            find prev ttCustomer  WHERE no-lock no-error.
            if available ttCustomer then do:

               do i = 1 to (frame-down - 1):
                  find prev ttCustomer WHERE no-lock no-error.
                  if available ttCustomer then memory = recid(ttCustomer).
                  else i = frame-down.
               end.
               must-print = true.
               next LOOP.
            end.
            else do:
               /* this is the first data page */
               bell.
               message "This is the 1st page !".          
               pause 1 no-message.
            end.
        end. /* previous page */

        /* next page */
        else if lookup(nap,"page-down,next-page") > 0 then do with frame sel:
           if rtab[frame-down] = ? then do:
               bell.
               message "This is the last page !".
               pause 1 no-message.
           end.
           else do: /* the downmost line wasn't empty */
               memory = rtab[frame-down].
               must-print = true.
               next LOOP.
           end.
        end. /* next page */

        /* Seek */
        if lookup(nap,"1,f1") > 0 then do:  /* CustNum */
           cfc = "puyr". run ufcolor.
           ehto = 9. run ufkey. ufkey = true.
           update CustNum with frame hayr.
           hide frame hayr no-pause.
           if CustNum ENTERED then do:
              find first ttCustomer where
                         ttCustomer.CustNum >= CustNum
              no-lock no-error.
               if not available ttCustomer then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
               end.
              /*  CustNum was found */
              assign
                memory = recid(ttCustomer)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first ttCustomer no-lock.

           memory = recid(ttCustomer).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last ttCustomer no-lock.

           memory = recid(ttCustomer).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "5" or nap = "f5" then do:
           find ttCustomer where recid(ttCustomer) = rtab[frame-line] no-lock.

           oiCustNum = ttCustomer.CustNum.
      
           leave main.
        
        end.
        ELSE IF nap = "4" OR nap = "f4" THEN DO:
            oiCustNum = 0 .
            LEAVE main.
        END.

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */

IF ilDisp THEN hide frame sel no-pause.
