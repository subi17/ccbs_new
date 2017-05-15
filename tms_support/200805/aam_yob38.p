{Syst/testpaa.i}
katun = "ari".

def var lccli      as char no-undo.
def var i          as int  no-undo.
def var j          as int  no-undo.

def stream sread.
input stream sread from /apps/snet/200805/delinv_yob38.txt.

def temp-table ttseq no-undo
   field pref as char
   field invnum as int format ">>>>>>>9"
   index pref pref.
   
repeat:

   import stream sread unformatted lccli.

   i = i + 1.
   
   if lccli = "" then next.
   
   find first msowner where msowner.cli = lccli no-lock no-error.
   if not available msowner then do:
      message "unknown cli:" lccli
      view-as alert-box.
      next.
   end.
   
   find first invoice use-index cli where 
              invoice.brand = "1" and
              invoice.cli   = lccli and
              invoice.invdate = 5/5/8 no-lock no-error.

   if not available invoice then next.
   
   if invoice.printstate > 0 or invoice.paymstate > 0 or invoice.ddstate > 0
   then do:
      message "check invoice:" invoice.invnum invoice.cli
      view-as alert-box.
      next.
   end.
   
   j = j + 1.
   display i j
           invoice.invnum
           invoice.cli
           invoice.invamt.

   find first ttseq where ttseq.pref = substring(invoice.extinvid,1,4)
   no-error.
   if not available ttseq then do:
      create ttseq.
      assign 
         ttseq.pref   = substring(invoice.extinvid,1,4)
         ttseq.invnum = integer(substring(invoice.extinvid,5)).
   end.
   
   ttseq.invnum = min(ttseq.invnum,integer(substring(invoice.extinvid,5))).
   
   RUN Inv/del_inv.p (invoice.invnum).
   
end.

for each ttseq:
    disp ttseq.
end.

