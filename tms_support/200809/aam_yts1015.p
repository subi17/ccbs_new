{Rate/rate_roamzone.i}
def var i as int no-undo.

find mobsub where cli = "622866585" no-lock.
disp clitype activationdate.

for each prepcdr no-lock use-index cli where
         prepcdr.cli = mobsub.cli and
         datest >= 9/1/8 /* and
         spocmt >= 51 and spocmt <= 53 */:

   i = i + 1. 
   disp i format ">>9"
        datest billcode ccn spocmt format ">>9" charge
        errorcode eventtype /* readints */. 

end.

for each mobcdr no-lock use-index cli where
         mobcdr.cli = mobsub.cli and
         datest >= 9/1/8 /* and
         spocmt >= 51 and spocmt <= 53 */:

   i = i + 1. 
   disp i format ">>9"
        datest string(timest,"hh:mm:ss") ccn spocmt format ">>9" amount
        errorcode eventtype /* readints */
         fGetMcdrDtlValue(mobcdr.datest,mobcdr.dtlseq,
                          "Original cdr type"). 

end.