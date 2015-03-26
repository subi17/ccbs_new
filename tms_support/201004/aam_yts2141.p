{testpaa.i}
katun = "qvantel".


def var i as int no-undo.
def var j as int no-undo.

def temp-table ttcli no-undo
   field cli as char 
   field qty as int 
   index cli cli .

def stream slog.

def buffer bcdr for mobcdr.


for each mobcdr no-lock use-index date where 
         mobcdr.datest = today and
         mobcdr.spocmt = 92,
   first mcdrdtl2 no-lock where
         mcdrdtl2.datest = mobcdr.datest and
         mcdrdtl2.dtlseq = mobcdr.dtlseq and
         entry(58,mcdrdtl2.detail,"|") begins "217.168.3":
         
   i = i + 1.
   
   if entry(58,mcdrdtl2.detail,"|") ne "217.168.3.1" then j = j + 1.
   
   pause 0.
   disp i j string(timest,"hh:mm:ss") with 1 down.

   /*
   disp mobcdr.cli string(timest,"hh:mm:ss")
        spocmt 
        entry(58,mcdrdtl2.detail,"|") format "x(15)".
   */     

   find first bcdr where recid(bcdr) = recid(mobcdr) exclusive-lock.
   bcdr.spocmt = 93.
   bcdr.rateccn = 93.

   find first ttcli where ttcli.cli = mobcdr.cli no-error.
   if not available ttcli then do:
      create ttcli.
      ttcli.cli = mobcdr.cli.
   end.
   ttcli.qty = ttcli.qty + 1.
   
end.         


output stream slog to /apps/yoigo/tms_support/201004/aam_yts2141.log append.

i = 0.
for each ttcli:

   i = i + 1.
   pause 0.
   disp i with 1 down.
   
   put stream slog unformatted
     ttcli.cli chr(9)
     ttcli.qty skip

end.

output stream slog close.


