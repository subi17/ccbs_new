def temp-table ttcall no-undo
   field gsmbnr as char 
   field bdest  as char
   field spocmt as int
   field datest as date
   field billcode as char
   field repccn   as int
   field dialtype as int
   field btype    as int
   field errorcode as int.

def temp-table ttprice no-undo
   field bdest as char
   field pricelist as char
   field price as dec
   index bdest bdest. 


FUNCTION fAnalBsub RETURNS LOGICAL

  (INPUT   b_sub      AS CHARACTER,
   OUTPUT  b_dest     AS CHARACTER, 
   OUTPUT  b_ccn      AS INTEGER,
   output  b_type     as int):

   DEF VAR b_len                     AS I    NO-UNDO.
   DEF VAR mod_bsub                  AS C    NO-UNDO.
   DEF VAR b                         AS I    NO-UNDO.
   DEF VAR dest_recid                AS RE   NO-UNDO INIT ?.
   DEF VAR nat_anumber               AS CHAR No-UNDO. 
   DEF VAR llPnpAllowed              AS LOG  NO-UNDO.
   DEF VAR lcbnet                    AS CHAR NO-UNDO .
   DEF VAR liOrigBType               AS INT  NO-UNDO.
   
   ASSIGN
      mod_bsub    = trim(b_sub)
      dest_recid  = ?
      lcbnet      = "".

   ASSIGN b_len      = LENGTH(mod_bsub).
   
   DO b = b_len TO 1 BY -1.

      IF CAN-FIND(first Bdest WHERE 
                  BDest.Brand    = "1"       AND 
                  Bdest.Bdest    = SUBSTR(mod_bsub,1,b))
      then do:

         FIND first Bdest WHERE 
              Bdest.Bdest      = SUBSTR(mod_bsub,1,b) AND 
              Bdest.Brand      = "1"       AND 
              Bdest.PNPCustNum = 0 
         NO-LOCK.

         ASSIGN 
         dest_recid = RECID(BDest). /* RECID FOR CASE 'GENERAL B' */
         LEAVE.
      END.   
   END.

   /************************************
   * IF dest_recid has now any VALUE   *
   * it means that either a private OR *
   * a common B-destination has been   *
   * found.  Read in that Bdest AND   *
   * get assiciated FIELD values.      *
   ************************************/

   IF dest_recid NE ? THEN DO:

      FIND BDest WHERE RECID(BDest) = dest_recid NO-LOCK.
         ASSIGN
         b_dest     = BDest.Bdest
         b_ccn      = BDest.CCN
         b_type     = bdest.desttype.
   END. 
   ELSE DO:  /* unknown destination (!)  */
      ASSIGN 
      b_dest     = ""
      b_ccn      = 0.
   END. 
   
END.     

def var lcfile as char no-undo.
def var lcline as char no-undo.
def var lcbdest as char no-undo.
def var lcplist as char no-undo.
def var ldprice as dec no-undo.
def var liccn as int no-undo.
def var i as int no-undo.

def temp-table ttfile no-undo
   field filename as char.
   
def stream sread.
def stream slog.

input stream sread through value("ls -1 /apps/snet/200901/aam_ycm1190*.txt").
repeat:
   import stream sread unformatted lcfile.
   
   if search(lcfile) ne ? then do:
      create ttfile.
      ttfile.filename = lcfile.
   end.
end.
input stream sread close.



for each ttfile:

   empty temp-table ttcall. 

   pause 0. 
   disp ttfile.filename format "x(60)" with frame a row 1 1 down.
   
   input stream sread from value(ttfile.filename).

   repeat:
      import stream sread unformatted lcline.
   
      lcbdest = entry(1,lcline,chr(9)).
   
      if index("0123456789",substring(lcbdest,1,1)) = 0 then next.
   
      if lcbdest begins "00" then lcbdest = substring(lcbdest,3).
      
      create ttcall.
      ttcall.gsmbnr = lcbdest.
   end.
   
   input stream sread close.

   output stream slog to value(replace(ttfile.filename,".txt",".log")).
   put stream slog unformatted
      "B-number"    chr(9)
      "Destination" chr(9)
      "Report CCN"  chr(9)
      "B-type"      chr(9)
      "Pricelist"   chr(9)
      "Tariff CCN"  chr(9)
      "Price"       skip.
      
   i = 0.
   
   for each ttcall:
      
      fanalbsub(INPUT  ttCall.GsmBnr,
                OUTPUT ttCall.Bdest,
                OUTPUT ttCall.repccn,
                output ttcall.btype).

      /*  
      disp ttcall.gsmbnr format "x(16)"
           ttCall.Bdest  format "x(16)"
           ttCall.repccn format ">>>>9"
           ttcall.btype  format ">>9" 
      with frame b row 6 42 down.
      */
         
      assign 
         lcplist = ""
         ldprice = 0
         liccn   = 0
         i = i + 1.
         
      pause 0.
      disp i with 1 down row 8.

      if ttcall.bdest > "" then   
      for each tariff no-lock where
               tariff.brand = "1" and
               tariff.bdest = ttcall.bdest and
               tariff.pricelist >= "common"
      by tariff.pricelist:
      
         assign
            lcplist = tariff.pricelist
            ldprice = tariff.price[1]
            liccn   = tariff.ccn.
         
         leave.
      end.         

      /*  
      disp lcplist format "x(12)" 
           liccn   format ">>>>" 
           ldprice format ">>>9.99999" when lcplist > "" with frame b.

      down with frame b.    
      */
       
      put stream slog unformatted
         ttcall.gsmbnr chr(9)
         ttCall.Bdest  chr(9).
         
      if ttcall.bdest > "" then put stream slog unformatted
         ttCall.repccn chr(9)
         ttcall.btype  chr(9).
      else put stream slog unformatted
         chr(9)
         chr(9).
         
      if lcplist > "" then put stream slog unformatted
         lcplist       chr(9)
         liccn         chr(9)
         string(ldprice,">>>9.99999").
           
      put stream slog skip.
         
   end.

   output stream slog close. 
   
end.
    

