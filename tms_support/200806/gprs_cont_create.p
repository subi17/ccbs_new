
def temp-table ttcli no-undo
    field clitype as char.

def var i       as int no-undo.
def var j       as int no-undo.
def var ldtfrom as date no-undo.
def var litime  as int  no-undo.

create ttcli.
ttcli.clitype = "cont".
create ttcli.
ttcli.clitype = "cont2".

find first daycampaign where
           daycampaign.brand = "1" and
           daycampaign.dcevent = "GPRS" no-lock.

for each ttcli,
    each mobsub no-lock where
         mobsub.brand = "1" and
         mobsub.clitype = ttcli.clitype:

      i = i + 1.
        
      if i mod 1000 = 0 then do:
         pause 0.
         disp i j with 1 down.
      end.
      
      if can-find(first dccli where 
                        dccli.msseq   = mobsub.msseq and
                        dccli.dcevent = daycampaign.dcevent and
                        dccli.validto >= today)
      then next.
      
      j = j + 1.

      ldtfrom = ?.
      
      for each msowner no-lock where
               msowner.msseq = mobsub.msseq and
               msowner.paytype = false 
      by msowner.tsbeg:
         Func.Common:mSplitTS(msowner.tsbeg,
                  output ldtfrom,
                  output litime).
         leave.
      end.

      if ldtfrom = ? then ldtfrom = mobsub.activationdate.  
      
      CREATE DCCLI.
      ASSIGN DCCLI.Brand        = "1"
             DCCLI.DCEvent      = daycampaign.dcevent
             DCCLI.MsSeq        = Mobsub.MsSeq
             DCCLI.CLI          = Mobsub.CLI
             DCCLI.ContractDate = ldtfrom
             DCCLI.ValidFrom    = ldtfrom
             DCCLI.ValidTo      = 12/31/2049
             DCCLI.CreateFees   = (DayCampaign.DCType = "3").
 
      CREATE Memo.
      ASSIGN
         Memo.CreStamp  = Func.Common:mMakeTS()
         Memo.Brand     = "1"
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CustNum   = MobSub.CustNum
         Memo.HostTable = "MobSub"
         Memo.KeyValue  = STRING(MobSub.MsSeq)
         Memo.CreUser   = "snet"
         Memo.MemoTitle = daycampaign.dcevent + " Created"
         Memo.MemoText  = "Periodical contract " + daycampaign.dcevent +
                          " created during a checkup of contracts " +
                          " to an existing subscription.".
end.

disp i j .
