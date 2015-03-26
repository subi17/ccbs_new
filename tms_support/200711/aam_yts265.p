{testpaa.i}
gcbrand = "1".
katun = "ari".
{timestamp.i}
{ftopup.i}

session:numeric-format = "european".

def stream slog.
output stream slog to /apps/snet/200711/aam_yts265_II.log append.

def var i      as int  no-undo.
def var j      as int  no-undo.
def var ldamt  as dec  no-undo.
def var ldvat  as dec  no-undo.
def var lczone as char no-undo.
def var lireq  as int  no-undo.

def buffer breq for prepaidrequest.


for each prepaidrequest no-lock where 
         prepaidrequest.brand    = "1"        and
         prepaidrequest.source   = "mincons"  and
         prepaidrequest.tsrequest >= 20071108 and
         prepaidrequest.tsrequest <  20071109 and
         prepaidrequest.ppstatus = 2          and
         prepaidrequest.respcode = 0
by prepaidrequest.tsrequest:         
         
   i = i + 1.

   ldamt = 0.
   ldvat = 0.
   lczone = "".
 
   for each breq no-lock use-index cli where
            breq.brand       = "1"                and
            breq.cli         = prepaidrequest.cli and
            breq.tsrequest  >= 20071108           and
            breq.tsrequest  <  20071109           and
            breq.source     = "mincons"           and 
            breq.ppstatus   = 2                   and
            breq.respcode   = 0                   and
            breq.pprequest > prepaidrequest.pprequest:
      assign 
        ldamt  = ldamt + breq.topupamt
        ldvat  = ldvat + breq.vatamt
        lczone = breq.taxzone.
   end.
            
   
   if ldamt ne 0 then do:
   
      if can-find(first breq use-index cli where
                        breq.brand       = "1"                and
                        breq.cli         = prepaidrequest.cli and
                        breq.tsrequest  >= 20071108           and
                        breq.tsrequest  <  20071109           and
                        breq.source     = "manfix"            and 
                        breq.topupamt   = -1 * ldamt)
      then next. 
      
      put stream slog unformatted
         prepaidrequest.cli   "|"
         /*
         trim(string((prepaidrequest.topupamt + ldamt) / 100,
                      "->>>>>>>>9.99"))  "|"
         */             
         trim(string(ldamt / 100,"->>>>>>>9.99")) "|".
         
      j = j + 1.   

      liReq = fAddTopUp(prepaidrequest.MsSeq,
                        prepaidrequest.CLI,
                        "RefillTRequest",
                        "MANFIX",
                        "",
                        "995",
                        lcZone,
                        ldAmt * -1,
                        ldVat * -1).
       
      put stream slog unformatted
         lireq skip.
   end.
         
   pause 0.
   disp i j with 1  down.
   
end.         
         



