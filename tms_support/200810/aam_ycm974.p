def stream slog.

/* 
   0=basic 
   1=term, no fee
   2=term, fee
   3=stc, no fee
   4=stc, fee
   5=cont,
   6=cont2,
   7=contrd1,
   8=cont4,
   9=fee,lang1 
   10=fee,lang2
   11=fee,lang3
   12=fee,lang5
   13=campaign fat
   14=fat
*/
   

def temp-table ttcase no-undo
    field cli as char
    field invcust as int
    field ttcase as int
    index ttcase ttcase.
    
def var licase as int no-undo.
def var ldfromper as dec no-undo.
def var ldtoper as dec no-undo.
def var lcmessage as char no-undo.
def var llfee as log no-undo.
def var licomper as int no-undo.
def var lccampfat as char no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var lcfound as char no-undo.
def var llcampaign as log no-undo.

def buffer breq for msrequest.

assign
   ldfromper = 20080901
   ldtoper   = 20080931.86400
   licomper  = 200809
   lccampfat = "CPFAT".

for each clitype no-lock where
         clitype.brand = "1" and
         clitype.paytype = 1:

   for each mobsub no-lock where
            mobsub.brand = "1" and
            mobsub.clitype = clitype.clitype:

      i = i + 1.
      
      pause 0.
      disp i j "mob" with 1 down.
          
      if not can-find(first invseq where 
                            invseq.msseq = mobsub.msseq and
                            invseq.billed = false)
      then next. 
      
      if can-find(first msrequest where 
                        msrequest.msseq = mobsub.msseq and
                        msrequest.reqtype = 0) or
         can-find(first msrequest where
                        msrequest.msseq = mobsub.msseq and
                        msrequest.reqtype = 18)
      then next.
         
      if can-find(first singlefee where
                        singlefee.brand = "1" and
                        singlefee.hosttable = "mobsub" and
                        singlefee.keyvalue = string(mobsub.msseq))
      then next.
      
      create ttcase.
      assign
         ttcase.cli = mobsub.cli
         ttcase.invcust = mobsub.invcust.
      
      case mobsub.clitype:
      when "cont" then ttcase.ttcase = 5.
      when "cont2" then ttcase.ttcase = 6.
      when "contrd1" then ttcase.ttcase = 7.
      when "cont4" then ttcase.ttcase = 8.
      end case.
        
      j = j + 1.
      leave.
   end.         
   
end.

llfee = false.
licase = 0.
lcfound = "".

for each msrequest no-lock where
         msrequest.brand = "1" and
         msrequest.reqtype = 0 and
         msrequest.reqstat = 2 and
         msrequest.actstamp >= 20081001 and
         msrequest.reqcparam1 ne "tarj3":

   if can-find(first breq where
                     breq.msseq = msrequest.msseq and
                     breq.reqtype = 18)
   then next.
 
   i = i + 1.
   pause 0.
   disp i j "stc" with 1 down.
 
   if can-find(first singlefee where
                     singlefee.brand = "1" and
                     singlefee.hosttable = "mobsub" and
                     singlefee.keyvalue = string(msrequest.msseq) and
                     singlefee.billcode = "termperiod" and
                     singlefee.billed = false)
   then llfee = true.                  
   else licase = 3.
            
   if llfee or licase > 0 then do:
   
      if llfee then licase = 4.
      if can-find(first ttcase where ttcase.ttcase = licase) then next.

      find first msowner no-lock where 
                 msowner.msseq = msrequest.msseq.
      create ttcase.
      assign 
         ttcase.cli = msowner.cli
         ttcase.invcust = msowner.invcust
         ttcase.ttcase = licase.
         lcfound = lcfound + string(ttcase.ttcase).
      j = j + 1.
   
      if index(lcfound,"3") > 0 and 
         index(lcfound,"4") > 0 then leave.
   end.
   

end.

llfee = false.
licase = 0.
lcfound = "".

for each msrequest no-lock where
         msrequest.brand = "1" and
         msrequest.reqtype = 18 and
         msrequest.reqstat = 2 and 
         msrequest.actstamp >= 20081001:

   i = i + 1.
   pause 0.
   disp i j "term" with 1 down.
 
   if can-find(first singlefee where
                     singlefee.brand = "1" and
                     singlefee.hosttable = "mobsub" and
                     singlefee.keyvalue = string(msrequest.msseq) and
                     singlefee.billcode = "termperiod" and
                     singlefee.billed = false)
   then llfee = true.                  
   else licase = 1.
            
   if llfee or licase > 0 then do:
   
      if llfee then licase = 2.
      if can-find(first ttcase where ttcase.ttcase = licase) then next.

      find first msowner no-lock where 
                 msowner.msseq = msrequest.msseq.
      create ttcase.
      assign 
         ttcase.cli = msowner.cli
         ttcase.invcust = msowner.invcust
         ttcase.ttcase = licase
         lcfound = lcfound + string(ttcase.ttcase).
      j = j + 1.   

      if index(lcfound,"1") > 0 and 
         index(lcfound,"2") > 0 then leave.

   end.

end.


lcfound = "".

for each singlefee no-lock use-index invnum_s where
         singlefee.invnum = 0 and
         singlefee.billcode = "termperiod" and
         singlefee.billed = false,
   first customer no-lock where
         customer.custnum = singlefee.custnum:

      i = i + 1.
      pause 0.
      disp i j "termlang" with 1 down.
      
      if index(lcfound,string(customer.language)) > 0 then next.
      
      if can-find(first msrequest where 
                        msrequest.msseq = integer(singlefee.keyvalue) and
                        msrequest.reqtype = 0) or
         can-find(first msrequest where
                        msrequest.msseq = integer(singlefee.keyvalue) and
                        msrequest.reqtype = 18)
      then next.
         
 
      case customer.language:
      when 1 then licase = 9.
      when 2 then licase = 10.
      when 3 then licase = 11.
      when 5 then licase = 12.
      otherwise licase = 0.
      end case.
       
      if licase = 0 then next.
      
      find first msowner no-lock where
                 msowner.msseq = integer(singlefee.keyvalue) and
                 msowner.invcust = singlefee.custnum.
                 
      create ttcase.
      assign 
         ttcase.cli = msowner.cli
         ttcase.invcust = singlefee.custnum
         ttcase.ttcase = licase.
      
      lcfound  = lcfound + string(customer.language).
      
      j = j + 1.

      if index(lcfound,"1") > 0 and 
         index(lcfound,"2") > 0 and
         index(lcfound,"3") > 0 and
         index(lcfound,"5") > 0
      then leave.

end.      
      

llcampaign = false.
licase = 0.
lcfound = "".

FOR EACH FaTime NO-LOCK WHERE
         FaTime.InvNum   = 0 and
         fatime.period   = licomper:

      i = i + 1.
      pause 0.
      disp i j "fat" with 1 down.


   /* is this related to a campaign */
    llCampaign = (FATime.FTGrp = lcCampFat).
         
    if not llcampaign then licase = 14.
    
    IF llCampaign or licase > 0 THEN do:
    
      if llcampaign then licase = 13.
      if can-find(first ttcase where ttcase.ttcase = licase) then next.

       find mobsub where mobsub.cli = fatime.cli no-lock no-error.
       if not available mobsub or mobsub.paytype = true then next.
       
       create ttcase. 
       assign 
          ttcase.cli = fatime.cli
          ttcase.invcust = mobsub.invcust
          ttcase.ttcase = licase.
         lcfound = lcfound + string(ttcase.ttcase).

      if index(lcfound,"13") > 0 and 
         index(lcfound,"14") > 0 then leave.

       j = j + 1.   

    end.

end.    


output stream slog to /tmp/test_doc1.txt.

for each ttcase:
      
   put stream slog unformatted
       ttcase.cli chr(9)
       ttcase.invcust chr(9)
       ttcase.ttcase skip.
end.

output stream slog close.

disp i j.
      


      
