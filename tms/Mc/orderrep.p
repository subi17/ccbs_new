/* ----------------------------------------------------------------------
  MODULE .......: orderrep.p
  TASK .........: report for orders
  APPLICATION ..: tms
  AUTHOR .......: tk
  CREATED ......: 24.09.03
  CHANGED ......: 03.11.03 tk checked status divided by credok
                  26.03.04 tk mtv3 version
                  
  Version ......: MTV3
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}

def var dfrom as da no-undo.
def var dto   as da no-undo.
def var fromstamp as de no-undo.
def var tostamp as de no-undo.
def var lkm as i no-undo.
def var ufkey as lo no-undo.
def var total as i no-undo.
def var rowtotal as i no-undo.

define temp-table statuscodes
   field statuscode as char
   field statusname as char format "x(20)"
   field count as i
   field mnp   as i
   index statuscode is primary statuscode.

create statuscodes.
assign
  statuscodes.statuscode = "TOTAL"
  statuscodes.statusname = "TOTAL".

FOR EACH TMSCodes NO-LOCK WHERE 
         TMSCodes.TableName = "Order" AND
         TMSCodes.FieldName = "StatusCode" AND
         TMSCodes.CodeGroup = "Orders":

   if tmscodes.codevalue = "" or tmscodes.codevalue = "9" then next.
   create statuscodes.
   assign
      statuscodes.statuscode = tmscodes.codevalue
      statuscodes.statusname = tmscodes.codename.

end.


form
 skip(2)
 "      This program will calculate amounts of orders by statuscode "
 "      from given period."
 skip(2)
 "         Date from ...:" dfrom no-label format "99-99-99" 
 "         Date to .....:" dto no-label format "99-99-99"
 skip(10)
with row 1 width 80 NO-LABELS
   title " " + ynimi + " ORDER REPORT " + string(pvm,"99-99-99") + " "
FRAME rajat.

form
   statuscodes.statusname column-label "Status"
   statuscodes.count      column-label "Own"
   statuscodes.mnp        column-label "MNP"
   rowtotal               column-label "Total"
with row 3 centered 12 down overlay title " Number of Orders "
   FRAME result.

assign 
dto = today
dfrom = today - 30.


loop:
repeat with frame rajat:
   PAUSE 0 no-message.
   ehto = 9. RUN ufkey.
   UPDATE 
   dfrom 
   dto.

   ASSIGN
      ufk = 0
      ufk[1] = 132
      ufk[5] = 63
      ufk[8] = 8
      ehto = 0
      ufkey = true.

   run ufkey.
   case toimi:
      when 8 then return.
      when 1 then next loop.
      when 5 then leave loop.
   end.

end.

if keylabel(lastkey) = "f4" then return.


assign 
   fromstamp = fHMS2TS(dfrom,"00:00:00")
   tostamp = fHMS2TS(dTo,"23:59:59").

for each statuscodes.
   assign
   statuscodes.count = 0
   statuscodes.mnp = 0.
end.

message "Calculating....".

for each order no-lock where
         order.brand = gcBrand and
         order.crstamp >= fromstamp and
         order.crstamp <= tostamp and
         order.tupas >= 2 and
         order.tupas <= 4:

   
      IF order.statuscode = "" then 
         FIND statuscodes WHERE statuscodes.statuscode = "1".
      ELSE FIND statuscodes WHERE statuscodes.statuscode = order.statuscode.

      IF order.mnpstatus = 0 then statuscodes.count = statuscodes.count + 1.
      else                        statuscodes.mnp   = statuscodes.mnp + 1.

      find statuscodes where statuscodes.statuscode = "TOTAL".

      IF order.mnpstatus = 0 then statuscodes.count = statuscodes.count + 1.
      else                        statuscodes.mnp   = statuscodes.mnp + 1.
     
end.


clear frame result.
for each statuscodes no-lock.
   disp 
      statuscodes.statusname
      statuscodes.count
      statuscodes.mnp
      statuscodes.count + statuscodes.mnp @ rowtotal
   with frame result.
   down with frame result.
end.

ASSIGN ufk = 0 ehto = 3. RUN ufkey.

message "Press ENTER to continue !".
pause no-message.
hide frame result.

