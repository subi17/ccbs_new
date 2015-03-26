def var i      as int  no-undo.
def var lcline as char no-undo.
def var lccli  as char no-undo.

def stream sread.
input stream sread from /apps/snet/200801/preactivated_sman_change.txt.

def temp-table ttcli no-undo
   field cli as char
   index cli cli.
   
repeat:
   import stream sread unformatted lcline.
   
   lccli = entry(1,lcline,"|").
   
   create ttcli.
   ttcli.cli = lccli.
   
   i = i + 1.
end.

input stream sread close.

message i "clis read"
view-as alert-box.

i = 0.

for each ttcli,
    each order exclusive-lock use-index cli where
         order.brand = "1" and
         order.cli   = ttcli.cli and
         order.source = "script" and
         order.clitype = "tarj3" and
         order.salesman = "pre-act":
         
   assign
      order.salesman     = "YOIGO"
      order.orderchannel = "PRE-ACT".
      
   i = i + 1.

   if i mod 1000 = 0 then do:
     pause 0.
     disp i with 1 down.
   end.

   find mobsub where mobsub.cli = ttcli.cli exclusive-lock.
   mobsub.salesman = "YOIGO".
   
end.

disp i.
