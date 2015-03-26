{commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{timestamp.i}

DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO. 
DEFINE VARIABLE plSimulated AS LOGICAL NO-UNDO. 
def buffer SIMbuf for SIM.
DEFINE STREAM sLog.
OUTPUT STREAM sLog TO 'yot_466.log'.

plSimulated = FALSE.
ldTS = fMakeTS(). 

FOR EACH SIMbuf EXCLUSIVE-LOCK WHERE 
         SIMbuf.Brand = gcBrand AND
         LOOKUP(SUBSTRING(SIMbuf.ICC,7,4) ,"0909,1009,1109,1209,0110") = 0 AND 
         SIMbuf.Stock = "RETAILER" AND 
         SIMbuf.SimStat = 1 :
        /* change status */
        IF NOT plSimulated THEN
           ASSIGN SIMbuf.SimStat = 7 .        
        put stream sLog unformatted SIMbuf.ICC skip.
END.
OUTPUT STREAM sLog CLOSE.

