{testpaa.i}
{timestamp.i}
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
         mobsub.clitype = "cont",
   first order no-lock where
         order.msseq = mobsub.msseq:

   i = i + 1.
   
   if i mod 1000 = 0 then do:
      pause 0.
      disp i j k with 1 down.
   end.
   
   if order.clitype = mobsub.clitype then 
      fsplitts(order.crstamp,
               output ldtorder,
               output litime).
   else ldtorder = 3/1/8.
            
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
   
   else if dccli.contractdate ne ldtbeg and dccli.contractdate >= 3/1/8 
   then do:
      k = k + 1.
      
      put stream slog unformatted
         dccli.msseq   chr(9)
         dccli.dcevent chr(9)
         dccli.contractdate chr(9)
         dccli.validfrom    chr(9)
         dccli.validto      chr(9)
         ldtbeg             skip.
         
      find bcli where recid(bcli) = recid(dccli) exclusive-lock.
      assign
         bcli.contractdate = ldtbeg
         bcli.validfrom    = ldtbeg.
         
      fwritememo(mobsub.msseq,
                 mobsub.custnum).
                 
      /*
      disp mobsub.cli 
           mobsub.activationdate 
           dccli.contractdate
           dccli.validfrom
           ldtbeg.
      */     
   end.
   
end.

for each msowner no-lock where
         msowner.brand   = "1"    and 
         msowner.clitype = "cont" and
         msowner.tsend > 20080301 and
         msowner.tsend < 20080401
break by msowner.msseq
      by msowner.tsend:

   if not first-of(msowner.msseq) then next.
   
   find first order no-lock where
              order.msseq = msowner.msseq no-error.
   if available order and order.clitype = msowner.clitype then do:
      fsplitts(order.crstamp,
               output ldtbeg,
               output litime).
   end.
   else do: 
      fsplitts(msowner.tsbeg,
               output ldtbeg,
               output litime).
   end.
   
   fsplitts(msowner.tsend,
            output ldtend,
            output litime).
   
   ldtbeg = max(ldtbeg,3/1/8).
             
   i = i + 1.
   
   for each bowner no-lock where
            bowner.msseq = msowner.msseq and
            bowner.tsend > msowner.tsend 
   by bowner.tsend:
   
      if bowner.clitype = msowner.clitype then do:
         if bowner.tsend >= 99999999 
         then ldtend = 12/31/2049.
         else fsplitts(bowner.tsend,
                       output ldtend,
                       output litime).
      end.
      else leave.
   end.
      
   find first dccli where
              dccli.brand   = "1" and
              dccli.dcevent = lcevent and
              dccli.msseq   = msowner.msseq no-lock no-error.
   
   if not available dccli then do:
   
      CREATE DCCLI.
      ASSIGN DCCLI.Brand        = "1"
             DCCLI.DCEvent      = lcevent
             DCCLI.MsSeq        = msowner.msseq
             DCCLI.CLI          = msowner.cli
             DCCLI.ContractDate = ldtbeg
             DCCLI.ValidFrom    = ldtbeg
             DCCLI.validto      = ldtend.

      put stream slog unformatted
         dccli.msseq   chr(9)
         dccli.dcevent chr(9)
         dccli.contractdate chr(9)
         dccli.validfrom    chr(9)
         dccli.validto      chr(9)
         "new"              skip.
 
      fwritememo(msowner.msseq,
                 msowner.custnum).
      
      j = j + 1.
   end.

   else if (dccli.contractdate ne ldtbeg or dccli.validto ne ldtend) and
        dccli.contractdate >= 3/1/8 and
        not (dccli.contractdate = ldtbeg and ldtend = 12/31/49 and 
             dccli.validto = 1/1/53)
   then do:
      k = k + 1.
      
      put stream slog unformatted
         dccli.msseq   chr(9)
         dccli.dcevent chr(9)
         dccli.contractdate chr(9)
         dccli.validfrom    chr(9)
         dccli.validto      chr(9)
         ldtbeg             chr(9)
         ldtend             skip.
         
      find bcli where recid(bcli) = recid(dccli) exclusive-lock.
      assign
         bcli.contractdate = ldtbeg
         bcli.validfrom    = ldtbeg
         bcli.validto      = ldtend.
         
      fwritememo(msowner.msseq,
                 msowner.custnum).
                 
      /*              
      disp msowner.cli 
           msowner.tsbeg
           msowner.tsend
           dccli.contractdate
           dccli.validfrom
           ldtbeg
           dccli.validto
           ldtend.
      */     
   end.
          
end.
         
         
disp i j k .

/* 
- order date vrs. activation date 
- osa alkanut 1.2.
- luonti requestilla vai suoraan
- tehdäänkö jos tapettu 1.3., mihin aikaan
*/
