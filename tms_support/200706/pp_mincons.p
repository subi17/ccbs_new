{Syst/testpaa.i}
katun = "snet".

{Func/timestamp.i}
{Func/ftaxdata.i}

def stream sread.
def stream sdone.
def stream serror.

FUNCTION fMinComp RETURNS LOGICAL
  (INPUT pcCLI     AS CHARACTER,
   INPUT pdeTS     AS DECIMAL,
   INPUT pdeCurBal AS DECIMAL):

   def var ldmincons as dec  no-undo.
   def var ldtaxperc as dec  no-undo.
   def var lctaxzone as char no-undo. 

   if can-find(first prepaidrequest use-index cli where
                     prepaidrequest.brand      = "1"   and
                     prepaidrequest.cli        = pcCLI and
                     prepaidrequest.tsrequest  = pdeTS and
                     prepaidrequest.source     = "mincons")
   then next. 
   
   find mobsub where mobsub.cli = pcCLI no-lock no-error.
   if not available mobsub then do:
      put stream serror unformatted
         pcCLI chr(9)
         "Subscription not found"
         skip.
      next.
   end.

   ldmincons = min(pdeCurBal,600).
   
   /* balance can't be set to zero */
   if ldmincons <= 1 then do:
      put stream serror unformatted
         pcCLI chr(9)
         "No balance"
         skip.
      next.
   end.
   
   find customer where customer.custnum = mobsub.custnum no-lock.
   
   ASSIGN 
      lcTaxZone = fRegionTaxZone(Customer.Region)
      ldTaxPerc = fTaxPerc(lcTaxZone,"1").
 
   CREATE PrePaidRequest.
   ASSIGN
      PrePaidRequest.Brand     = "1"
      PrePaidRequest.CLI       = pcCLI
      PrePaidRequest.PPRequest = INT("996" + STRING(NEXT-VALUE(PrePaidReq)))
      PrePaidRequest.Request   = "AdjustmentTRequest" 
      PrePaidRequest.CommLine  = "AdjustmentTRequest" 
      PrePaidRequest.Source    = "MINCONS"
      PrePaidRequest.PPStatus  = 0
      PrePaidRequest.TSRequest = pdeTS
      PrePaidRequest.TopUpAmt  = -1 * ldmincons 
      PrePaidRequest.VatAmt    = ldTaxPerc * PrePaidRequest.TopUpAmt / 100
      PrePaidRequest.TaxZone   = lcTaxZone.

      /*  
      PrepaidRequest.PPStatus   = 0.
      */

   CREATE Memo.
   ASSIGN
      Memo.Brand     = gcBrand
      Memo.HostTable = "MobSub"
      Memo.KeyValue  = STRING(MobSub.MsSeq)
      Memo.CustNum   = MobSub.CustNum
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = katun 
      Memo.MemoTitle = "Minimum Consumption"
      Memo.MemoText  = "Subscription's balance has been charged with " +
                       string(ldmincons) + 
                       " euros Minus adjustment as minimum consumption."
      Memo.CreStamp  = pdeTS.
          
   put stream sdone unformatted
      pcCLI     chr(9)
      pdeCurBal chr(9)
      ldmincons chr(10).
   
   RETURN TRUE.
   
END.

def var lcline    as char no-undo.
def var lccli     as char no-undo.
def var liLoop    as int  no-undo.
def var liCliAmt  as int  no-undo.

input  stream sread  from /apps/snet/200706/pp_mincons.txt.
output stream sdone  to   /apps/snet/200706/pp_mincons_done.log  append.
output stream serror to   /apps/snet/200706/pp_mincons_error.log append.

repeat:

   import stream sread unformatted lcline.

   DO liLoop = 1 TO NUM-ENTRIES(lcLine,";"):

      IF ENTRY(liLoop,lcline,";") NE "" THEN DO:
      
         ASSIGN
            lccli    = ENTRY(liLoop,lcline,";")
            liCliAmt = liCliAmt + 1.
         
         PUT SCREEN ROW 1 COL 1 STRING(liCliAmt).
         
         RUN Gwy/balancequery.p(lccli).
         
         fMinComp(lccli,fMakeTS(),DECIMAL(RETURN-VALUE)).

      END.

   END.

end.

input stream sread  close.
input stream sdone  close.
input stream serror close.

