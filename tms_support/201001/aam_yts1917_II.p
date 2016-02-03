{Syst/commpaa.i}
gcbrand = "1".
katun = "ari".
{Func/timestamp.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}
END.


def var i as int no-undo.
def var j as int no-undo.
def var lcline as char no-undo.
def var limsseq as int no-undo.
def var ldamt as dec no-undo.
def var liinv as int no-undo.
def var lccli as char no-undo.
def var ldtaxamt as dec no-undo.
def var lccreditnote as char no-undo.
def var ldaend as date no-undo.

def buffer breq for msrequest. 


function fenddate returns date
   (idafrom as date):
   
   def var liyear as int no-undo.
   def var limonth as int no-undo.
   def var lidate as int no-undo.
   
   ASSIGN 
      liYear  = TRUNC(18 / 12,0) 
      liMonth = 18 MOD 12 
      liYear  = YEAR(idafrom) + liYear
      liMonth = MONTH(idafrom) + liMonth
      liDate  = DAY(idafrom).
             
   IF liMonth > 12 THEN ASSIGN 
      liYear  = liYear + 1
      liMonth = liMonth - 12.
        
   IF liMonth = 2 AND liDate > 28
   THEN liDate = DAY(DATE(3,1,liYear) - 1).
   ELSE IF liDate = 31 AND liMonth < 12 
   THEN liDate = DAY(DATE(liMonth + 1,1,liYear) - 1).
      
   return DATE(liMonth,liDate,liYear) - 1.
   
end function.


def stream sread.
input stream sread from /apps/snet/201001/aam_yts1914.log.

def stream slog.
output stream slog to /apps/snet/201001/aam_yts1917_percontr.log append.

put stream slog unformatted
   "MSISDN"     ";"
   "Subscr.ID"  ";"
   "Customer"   ";"
   "Contract"   ";"
   "From"       ";"
   "To"         ";"
   "New To"     ";"
   "Request"    skip.
   
repeat:

   import stream sread unformatted lcline.

   assign 
      limsseq = integer(entry(1,lcline,chr(9)))
      lccli   = entry(2,lcline,chr(9))
      ldamt   = decimal(entry(6,lcline,chr(9)))
      no-error.
   
   if error-status:error then next.

   i = i + 1.

   find first dccli where
              dccli.msseq = limsseq and
              dccli.dcevent = "postrenove" and
              dccli.validto = 12/31/9 no-lock no-error.
  
   if not available dccli then next.
   
   find first msrequest where
              msrequest.msseq = limsseq and
              msrequest.reqtype = 9 and
              msrequest.reqstat = 2 and
              msrequest.actstamp >= 20091231 and
              msrequest.actstamp < 20100101 and 
              msrequest.reqcparam3 = "postrenove" and
              msrequest.createfees = true no-lock no-error.
   if not available msrequest then next.
   
   ldaend = fenddate(dccli.validfrom).
   
   j = j + 1.
   disp i  
        j 
        limsseq format ">>>>>>>>>>9"
        lccli format "x(10)"
        dccli.validfrom 
        dccli.validto
        ldaend format "99-99-99".
   
   put stream slog unformatted
     lccli    ";"
     limsseq  ";"
     msrequest.custnum ";"
     dccli.dcevent ";"
     dccli.validfrom ";"
     dccli.validto  ";"
     ldaend ";"
     msrequest.msrequest skip.
     
   find current dccli exclusive-lock.
   assign 
      dccli.termdate = ?
      dccli.validto = ldaend.
      
   find current msrequest exclusive-lock.
   msrequest.reqstat = 4. 
   
end.

disp i j.

input stream sread close.
output stream slog close.

