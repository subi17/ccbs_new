def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.

def var lcline as char no-undo.
def var lccli  as char no-undo.
def var lcorgid as char no-undo.
def var licust as int  no-undo.
def var limsseq as int no-undo.
def var lidone as int no-undo.
def var ldatodate as date no-undo.

def stream sread.
input stream sread from /apps/snet/200909/LISTA_MSISDN_CODIGO-QUANTEL.csv.

def stream slog.
output stream slog to /apps/snet/200909/aam_yot160.log append.

repeat:
   import stream sread unformatted lcline.
   
   assign 
      lcorgid = entry(1,lcline,";")
      lccli  = entry(2,lcline,";")
      no-error.
     
   if error-status:error then next.
   

   if lccli = "" or lcorgid = "dni" then next.
   
   i = i + 1. 
   
   limsseq = 0.
   
   find first mobsub where mobsub.cli = lccli no-lock no-error.
   if available mobsub then do:
   
      j = j + 1.

      find first customer where customer.custnum = mobsub.agrcust
        no-lock no-error.

      /*
      if customer.orgid ne lcorgid then do:
         k = k + 1.
         put stream slog unformatted
            lcline ";"
            "Invalid DNI" 
            skip.
      end.
      else 
      */
      assign
         limsseq = mobsub.msseq
         licust  = mobsub.invcust.
   end.

   else do:
      
      for each termmobsub no-lock where termmobsub.cli = lccli,
         first customer no-lock where
               customer.custnum = termmobsub.agrcust and
               customer.orgid   = lcorgid:
         j = j + 1.
         limsseq = termmobsub.msseq.
         licust = termmobsub.invcust.
         leave.
      end.

      if limsseq = 0 then do:
         k = k + 1.
         put stream slog unformatted
            lcline ";"
            "No subscription found" 
            skip.
      end.      
   end.

   if limsseq = 0 then  next. 
   
   if not can-find(first invattach where
                         invattach.custnum  = licust and
                         invattach.attach4  = limsseq and
                         invattach.todate >= 10/1/9)
   then do:
      lidone = lidone + 1.
      
      ldatodate = 10/31/9.
      do while true:
         if not can-find(first invattach where
                            invattach.custnum  = licust and
                            invattach.todate   = ldatodate)
         then leave.
         ldatodate = ldatodate - 1.
         if ldatodate < 10/10/9 then do:
            message "customer" licust 
                    "has more than 20 subscriptions"
            view-as alert-box.
            ldatodate = 11/30/9.
         end.
      end.
                         

      create invattach.
      assign
         invattach.custnum  = licust
         invattach.todate   = ldatodate
         invattach.fromdate = 10/1/9
         invattach.attach2  = 1 
         invattach.attach4  = limsseq.
   end.

   pause 0.
   disp i j k lidone with 1 down.

end.

input stream sread close.
output stream slog close.

disp i j k lidone .


   

