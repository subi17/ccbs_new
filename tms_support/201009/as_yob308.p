find mobsub where
   mobsub.cli = "622652896" NO-LOCK.


def buffer bffitem for ffitem.
FOR first MServiceLimit EXCLUSIVE-LOCK WHERE
         MServiceLimit.msseq = mobsub.msseq and
         MServiceLimit.slseq = 5:

     MServiceLimit.fromts = 20100901.00000.
   
   find fixedfee EXCLUSIVE-LOCK where
      fixedfee.brand = "1"  and
      fixedfee.custnum = MobSub.custnum and
      fixedfee.hosttable = "mobsub" and
      fixedfee.keyvalue = string(mobsub.msseq) and
      fixedfee.begperiod = 201010 and
      fixedfee.billcode = "contdatamf".

   assign 
      fixedfee.begperiod = 201009
      fixedfee.begdate = 1/9/2010. 

  find first ffitem of fixedfee NO-LOCK no-error.

   create bffitem.
   assign
      bffitem.ffitemnum = next-value(citem)
      bffitem.billperiod = 201009
      bffitem.concerns[1] = 20100901
      bffitem.concerns[2] = 20100930.
   buffer-copy ffitem except ffitem.ffitemnum ffitem.concerns to bffitem.
  
end.
