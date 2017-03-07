{Syst/testpaa.i}
katun = "qvantel".


def var i as int no-undo.
def var j as int no-undo.

def stream slog.

def stream sread.
input stream sread from /home/ari/work/aam_yts2141.log.

def var lcline as char no-undo.
def var lccli  as char no-undo.
def var lcqty as char no-undo.


output stream slog to /apps/yoigo/tms_support/201004/aam_yts2141.log append.

i = 0.

repeat:

   import stream sread unformatted lcline.

   lccli = entry(1,lcline,chr(9)).
   lcqty = entry(2,lcline,chr(9)).

   i = i + 1.
   pause 0.
   disp i lccli with 1 down.
   
   put stream slog unformatted
     lccli chr(9)
     lcqty chr(9)
     "rerate"  skip.

   RUN Rate/cli_rate.p (lccli,
                   4/1/10,
                   4/30/10,
                   true).

end.

output stream slog close.


