{timestamp.i}

def var i       as int  no-undo.
def var j       as int  no-undo.
def var k       as int  no-undo.
def var ldtdate as date no-undo.
def var litime  as int  no-undo.
def var licust  as int  no-undo.
def var lcvouch as char no-undo.
def var ldperc  as dec  no-undo.
def var limonth as int  no-undo.

def temp-table ttsum no-undo
   field month    as int
   field vatperc  as dec
   field vatamt   as dec
   index month month vatperc.
   
session:numeric-format = "european".

def stream slog.


for each prepaidrequest no-lock where 
         prepaidrequest.brand  = "1"       and
         prepaidrequest.source = "mincons" and
         round(prepaidrequest.vatamt / 100,2) ne 0 and 
         prepaidrequest.tsrequest > 20070701:

    i = i + 1.

    if j = 0 then do:
       k = k + 1.
      
       output stream slog to value("/apps/snet/200710/aam_yoi450_" +
                                  string(k) + ".log").

       put stream slog unformatted
          "Customer"    chr(9)
          "MSISDN"      chr(9)
          "Date"        chr(9)
          "Topup"       chr(9)
          "Tax"         chr(9)
          "Voucher"     skip.
          
       j = 1.   
    end.

    if prepaidrequest.vatamt / prepaidrequest.topupamt < 0 then do:

       fsplitts(prepaidrequest.tsrequest,
                output ldtdate,
                output litime).
                
       assign
          licust  = 0
          lcvouch = "".
       
       find first payment use-index refnum where
                  payment.brand   = "1"               and
                  payment.refnum  = string(pprequest) and
                  payment.accdate = ldtdate
       no-lock no-error.
       if available payment then assign
          licust  = payment.custnum
          lcvouch = payment.extvoucher.
       
       else for first msowner no-lock where
                      msowner.cli    = prepaidrequest.cli       and
                      msowner.tsend >= prepaidrequest.tsrequest and
                      msowner.tsbeg <= prepaidrequest.tsrequest:
          licust = msowner.custnum.
       end.
       
       j = j + 1.
       
       put stream slog unformatted
          licust                               chr(9)
          prepaidrequest.cli                   chr(9)
          ldtdate                              chr(9)
          prepaidrequest.topupamt / 100        chr(9)
          round(prepaidrequest.vatamt / 100,2) chr(9)
          lcvouch                              skip.

       if j > 50000 then do:
       
          j = 0.
          output stream slog close.
       end.   
        
       assign
          limonth = year(ldtdate) * 100 + month(ldtdate)
          ldperc  = round(-100 * prepaidrequest.vatamt / 
                          prepaidrequest.topupamt,0).
                          
       find first ttsum where
                  ttsum.month   = limonth and
                  ttsum.vatperc = ldperc no-error.
       if not available ttsum then do:
          create ttsum.
          assign ttsum.month   = limonth
                 ttsum.vatperc = ldperc.
       end.

       ttsum.vatamt = ttsum.vatamt + round(prepaidrequest.vatamt / 100,2).
    
    end.
   
    if i mod 1000 = 0 then do:
       pause 0.
       disp i j prepaidrequest.tsrequest with 1 down.
    end.
end.

disp i j.

output stream slog close.

output stream slog to /apps/snet/200710/aam_yoi450_sum.log.

put stream slog unformatted
   "Month"     chr(9)
   "Tax%"      chr(9)
   "Posted"    chr(9)
   "Correct Posting" skip.


for each ttsum:

   put stream slog unformatted
      ttsum.month   chr(9)
      ttsum.vatperc chr(9)
      ttsum.vatamt  chr(9)
      -1 * ttsum.vatamt skip.
end.

output stream slog close.




