def temp-table ttvalue no-undo
   field hontitle as char
   field qty as int
   index hontitle hontitle.
   
def var i as int no-undo.
def var j as int no-undo.

def stream slog.
output stream slog to /home/ari/work/yts1925_changed.log append.

def buffer bcust for customer.

for each customer no-lock:

    if hontitle > "" then do:
       find first ttvalue where ttvalue.hontitle = customer.hontitle no-error.
       if not available ttvalue then do:
          create ttvalue.
          ttvalue.hontitle = customer.hontitle.
       end.
       ttvalue.qty = ttvalue.qty + 1.
    end.
    
    i = i + 1.
    if i mod 10000 = 0 then do:
       pause 0.
       disp i j with 1 down.
    end.
    
    if customer.hontitle = "snr." then do:
       j = j + 1.
               
       put stream slog unformatted 
          customer.custnum  chr(9)
          customer.hontitle skip.
          
       find first bcust where recid(bcust) = recid(customer) exclusive-lock.
       bcust.hontitle = "Sra.".
    end.
 end.
 
 for each ttvalue:
    disp ttvalue.
 end.
