{Func/penaltyfee.i}

def stream slog.
output stream slog to /apps/snet/200907/aam_ycm1658.log append.

def var lctac  as char no-undo.
def var lcread as char no-undo.
def var i      as int  no-undo.
def var j      as int  no-undo.
def var k      as int  no-undo.
def var lcnew  as char no-undo.
def var lccurr as char no-undo.
def var llbilled as log  no-undo.
def var ldfactor as dec  no-undo.
def var ldaend   as date no-undo.
def var lireq    as int  no-undo.

function fenddate returns date
   (idadate as date,
    iimonths as int):

   def var liyear as int no-undo.
   def var limonth as int no-undo.
   def var lidate as int no-undo.
   
   ASSIGN 
      liYear  = TRUNC(iimonths / 12,0) 
      liMonth = iimonths MOD 12 
      liYear  = YEAR(idadate) + liYear
      liMonth = MONTH(idadate) + liMonth
      liDate  = DAY(idadate).
             
   IF liMonth > 12 THEN ASSIGN 
      liYear  = liYear + 1
      liMonth = liMonth - 12.
        
   IF liMonth = 2 AND liDate > 28
   THEN liDate = DAY(DATE(3,1,liYear) - 1).
   ELSE IF liDate = 31 AND liMonth < 12 
   THEN liDate = DAY(DATE(liMonth + 1,1,liYear) - 1).
      
   return DATE(liMonth,liDate,liYear) - 1.
end function.


def buffer bcontr for dccli.
def buffer border for order.

assign
   lccurr = "TERM18II"
   lcnew  = "TERM18".    

for each order no-lock use-index stamp where
         order.brand = "1" and
         order.crstamp > 20090701 and
         order.crstamp < 20090801 and
         order.orderchannel = "pos" and
         order.clitype = "contrd1",
   first msowner no-lock where
         msowner.msseq = order.msseq,
   first dccli no-lock where
         dccli.msseq = order.msseq and
         dccli.dcevent = lcCurr:
         
   if dccli.validto < today then do:
      llbilled = false. 
      for first singlefee no-lock use-index hosttable where
                singlefee.brand = "1" and
                singlefee.hosttable = "mobsub" and
                singlefee.keyvalue = string(order.msseq) and
                singlefee.billcode = "termperiod":
         llbilled = singlefee.billed.
      end.
      if llbilled then next. 
   end.
   
   i = i + 1.
   if not can-find(first mobsub where mobsub.msseq = order.msseq) then 
      j = j + 1.

   /*
   disp order.crstamp order.orderid 
        order.statuscode
        order.msseq
        order.offer.
   next.
   */
   
   pause 0.
   disp i j k with 1 down.
   
   put stream slog unformatted
      order.msseq     chr(9)
      order.cli       chr(9)
      order.offer     chr(9)
      dccli.dcevent   chr(9)
      dccli.validfrom chr(9)
      dccli.validto   chr(9).
     
   find first bcontr where recid(bcontr) = recid(dccli) exclusive-lock.
   bcontr.dcevent = lcnew.
      
   find first border where recid(border) = recid(order) exclusive-lock.
   if border.offer = "G0000002IPL" then
      border.offer = "G0000001IPL".
   else if border.offer = "G0000002IPLMNP" then  
      border.offer = "G0000001IPLMNP".
   
   lireq = 0.
   for first msrequest exclusive-lock where
             msrequest.msseq   = order.msseq and
             msrequest.reqtype = 8 and
             msrequest.reqstat = 2 and
             msrequest.reqcparam3 = lcCurr:
      assign
         msrequest.reqcparam3 = lcnew
         lireq = msrequest.msrequest.
   end.

   put stream slog unformatted
      lireq chr(9).

   lireq = 0.
   for first msrequest exclusive-lock where
             msrequest.msseq   = order.msseq and
             msrequest.reqtype = 9 and
             msrequest.reqstat = 2 and
             msrequest.reqcparam3 = lcCurr:
      assign
         msrequest.reqcparam3 = lcnew
         lireq = msrequest.msrequest.
   end.

   put stream slog unformatted
      lireq chr(9).

   for first singlefee exclusive-lock use-index hosttable where
             singlefee.brand = "1" and
             singlefee.hosttable = "mobsub" and
             singlefee.keyvalue = string(order.msseq) and
             singlefee.billcode = "termperiod" and
             singlefee.billed = false:

         put stream slog unformatted
            singlefee.fmitemid chr(9)
            singlefee.amt      chr(9).
            
         ldaend = fenddate(dccli.validfrom,18).
         ldfactor = fCalculateFactor(dccli.validfrom,
                                     ldaend,
                                     dccli.validto,
                                     2).
         singlefee.amt = truncate(ldfactor * 100,0). 

         put stream slog unformatted
            singlefee.amt.
         
         k = k + 1.
   end.
   
   put stream slog skip.
   
end.         

for each order no-lock use-index stamp where
         order.brand = "1" and
         order.crstamp > 20090701 and
         order.crstamp < 20090801 and
         order.orderchannel = "pos" and
         order.clitype = "contrd1" and
         lookup(order.statuscode,"12,20,73") > 0:
         
   i = i + 1.

   pause 0.
   disp i j k with 1 down.
   
   put stream slog unformatted
      order.msseq     chr(9)
      order.cli       chr(9)
      order.offer     skip.
     
   find first border where recid(border) = recid(order) exclusive-lock.
   if border.offer = "G0000002IPL" then
      border.offer = "G0000001IPL".
   else if border.offer = "G0000002IPLMNP" then  
      border.offer = "G0000001IPLMNP".
end.
 
output stream slog close.

disp i j k.
 
