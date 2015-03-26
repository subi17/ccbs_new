def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.

def stream slog.
output stream slog to /apps/snet/200709/aam_yts169_VI.log append.

def buffer bcdr for mobcdr.

for each invseq no-lock where
         invseq.billed = false and
         invseq.msseq > 0 and
         invseq.fromdate < 9/1/7,
    each mobcdr no-lock where
         mobcdr.invcust = invseq.custnum and
         mobcdr.invseq  = invseq.invseq:

   i = i + 1.

   if mobcdr.datest < 6/1/7 then do:
   
      j = j + 1.
  
      export stream slog mobcdr.
      find bcdr where recid(bcdr) = recid(mobcd) exclusive-lock.
      bcdr.errorcode = 8049.
      bcdr.invseq    = 0.

   end.
   
   if i mod 1000 = 0 then do:
      pause 0.
      disp i j mobcdr.datest  with 1 down row 1.
   end.
end.

i = 0.
j = 0.

for each invoice no-lock where
         invoice.brand = "1" and
         invoice.invdate = 9/13/7 and
         invoice.invtype = 1:

   k = k + 1.
   pause 0.
   disp k with 1 down row 8.
   
   for each invseq no-lock where
            invseq.billed = false         and
            invseq.msseq  = invoice.msseq and
            invseq.fromdate < 9/1/7,
       each mobcdr no-lock where
            mobcdr.invcust = invseq.custnum and
            mobcdr.invseq  = invseq.invseq:

      i = i + 1.

      if mobcdr.datest < 9/1/7 then do:
   
         j = j + 1.

         disp invoice.cli
              invoice.custnum
              invoice.extinvid
              invseq.fromdate
              invseq.todate
              mobcdr.datest
              mobcdr.spocmt
              mobcdr.amount
              mobcdr.readints
              mobcdr.errorcode.
         pause.             
               /*
         export stream slog mobcdr.
         find bcdr where recid(bcdr) = recid(mobcd) exclusive-lock.
         bcdr.errorcode = 8049.
         bcdr.invseq    = 0.
         */
      end.
   
      if i mod 1000 = 0 then do:
         pause 0.
         disp "Inv" i j mobcdr.datest  with 1 down row 16.
      end.
   end.
      
end.

         
 
disp i j.


