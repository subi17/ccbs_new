{Syst/testpaa.i}
{Func/timestamp.i}
katun = "ari".

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.

def var lcmemo    as char no-undo.
def var ldcurrent as dec  no-undo.
def var lcevent   as char no-undo.
def var ldtbeg    as date no-undo.
def var ldtend    as date no-undo.
def var litime    as int  no-undo.
def var ldtorder  as date no-undo.

def buffer bcli   for dccli.
def buffer bowner for msowner.

def stream slog.
output stream slog to /apps/snet/200803/aam_yoi734.log append.

lcmemo = 
   "YOIGOYOIGO periodical contract creation and/or contract data " + 
   "update due to Yoigo's CONTRATO1 tariff plan change 1st of March 2008.".

assign
   ldcurrent = fmakets()
   lcevent   = "YOIGOYOIGO".


function fwritememo returns logic
   (iimsseq as int,
    iicust  as int):
    
   CREATE Memo.
   ASSIGN Memo.Brand     = gcBrand
          Memo.HostTable = "MobSub"
          Memo.KeyValue  = STRING(iiMsSeq)
          Memo.CustNum   = iiCust
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = katun 
          Memo.MemoTitle = lcEvent
          Memo.MemoText  = lcMemo
          Memo.CreStamp  = ldCurrent.
     
end function.

for each mobsub no-lock where
         mobsub.brand = "1" and
         mobsub.clitype = "cont" and
         mobsub.cli = "622000241":

   i = i + 1.
   
   ldtorder = 3/1/8.
            
   ldtbeg = max(ldtorder,3/1/8).
   
   /* clitype changed after 1.3. */
   for each msowner no-lock where
            msowner.msseq = mobsub.msseq and
            msowner.tsend > 20080302
   by msowner.tsend desc:
      
      if msowner.clitype ne mobsub.clitype then do:
         fsplitts(msowner.tsend,
                  output ldtbeg,
                  output litime).
         leave.         
      end.
   end.      

   find first dccli where
              dccli.brand   = "1" and
              dccli.dcevent = lcevent and
              dccli.msseq   = mobsub.msseq no-lock no-error.
   
   if not available dccli then do:
      j = j + 1.

      CREATE DCCLI.
      ASSIGN DCCLI.Brand        = "1"
             DCCLI.DCEvent      = lcevent
             DCCLI.MsSeq        = mobsub.msseq
             DCCLI.CLI          = mobsub.cli
             DCCLI.ContractDate = ldtbeg
             DCCLI.ValidFrom    = ldtbeg
             DCCLI.validto      = 12/31/49.
 
      put stream slog unformatted
         dccli.msseq   chr(9)
         dccli.dcevent chr(9)
         dccli.contractdate chr(9)
         dccli.validfrom    chr(9)
         dccli.validto      chr(9)
         "new"              skip.
 
      fwritememo(mobsub.msseq,
                 mobsub.custnum).
   end.
   
end.

