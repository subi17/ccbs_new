{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/date.i}

DEFINE VARIABLE liRangeStart AS INTEGER NO-UNDO. 
DEFINE VARIABLE liRangeEnd   AS INTEGER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream slog.
output stream slog to /apps/snet/200810/as_ycm1037.errors2.txt.

def stream screated.
output stream screated to /apps/snet/200810/as_ycm1037.created2.txt.

DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
ldeNow = fmakets().

FUNCTION fCreateMSIDNStamp RETURNS LOGICAL
(iiBegin AS INTEGER,
 iiEnd AS INTEGER):

   DO i = iiBegin to iiEnd:
      
      FIND FIRST msisdn where 
         msisdn.brand = "1" and
         msisdn.cli   = string(i) NO-LOCK NO-ERROR.
      
      IF AVAIL msisdn then do:
         export stream slog msisdn. 
      end.
      else do:
        
        FIND FIRST msisdnnumber where
         msisdnnumber.cli = string(i) NO-LOCK NO-ERROR.
        
        IF NOT AVAIL msisdnnumber then MESSAGE string(i) VIEW-AS ALERT-BOX.
         
         CREATE MSISDN.
         ASSIGN
            MSISDN.Brand      = gcBrand
            MSISDN.CLI        = msisdnnumber.cli
            MSISDN.ValidFrom  = ldeNow 
            MSISDN.POS        = ""
            MSISDN.Statuscode = 0
            MSISDN.ValidTo    = 99999999.99999
            MSISDN.ActionDate = Today.
         
         put stream screated unformatted msisdnnumber.cli skip.
         release msisdn.
      end.
     
   END.

END FUNCTION. 

/*
fCreateMSIDNStamp(633000000, 633000999).
fCreateMSIDNStamp(633997000, 633999999).
*/
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.
input from /apps/snet/200810/as_ycm1037.extra_nums.txt.
repeat.
   import unformatted lcline.
   fCreateMSIDNStamp(int(lcLine), int(lcLine)).
end.


output stream slog close.
output stream screated close.
