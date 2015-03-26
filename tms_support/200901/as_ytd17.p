def stream sout.
output stream sout to /apps/snet/200901/as_ytd17.log.
DEFINE VARIABLE liCli AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcNewStock AS CHAR NO-UNDO. 

DEF BUFFER msisdnbuf for msisdn.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

LOOPPI:
FOR EACH msisdn WHERE
  msisdn.brand = "1"  
 NO-LOCK USE-INDEX CLI BREAK BY msisdn.cli:

   IF FIRST-OF(msisdn.cli) THEN DO: 
   
      lcNewStock = "".
      liCli = int(msisdn.cli).
/*
      i = i + 1.
      if i > 10000 then leave looppi.
*/      
      if lookup(msisdn.pos,"cc,gift,pos,webshop,telem,telemark") > 0 THEN DO:
         lcNewStock = "ONLINE".
      end.
      else if (liCli >= 633000000 and liCli <= 633009999) or
           (liCli >= 633980000 and liCli <= 633999999) or
           (liCli >= 633600000 and liCli <= 633632999) or
           (liCli >= 633634000 and liCli <= 633699999) then do:
           lcNewStock = "VIP".
      end.
      else if (liCli >= 633100000 and liCli <= 633599999) THEN do:
        
         if msisdn.pos = "VIP" THEN DO:
            find msisdnnumber where
               msisdnnumber.cli = msisdn.cli NO-LOCK NO-ERROR.
            IF AVAIL msisdnnumber and 
               (msisdnnumber.rank eq 1 or msisdnnumber.rank eq 2) then do:
                  put stream sout unformatted msisdn.cli "|" msisdn.pos "|"
                     "SKIPPED" skip.
                  next LOOPPI.
            END.
         END.

         lcNewStock = "NOTPUBLIC".

      end.  

      if lcNewStock NE "" AND lcNewStock NE msisdn.pos THEN DO:
         put stream sout unformatted msisdn.cli "|" msisdn.pos "|"
            lcNewStock skip.
         
           do trans:
           find msisdnbuf where rowid(msisdnbuf) = rowid(msisdn) EXCLUSIVE-LOCK.
           assign msisdnbuf.pos = lcNewStock.
           release msisdnbuf.
           end.
          
      END.

   END.    
END.

output stream sout close.
