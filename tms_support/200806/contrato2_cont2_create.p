{timestamp.i}

def temp-table ttcli no-undo
    field clitype as char.

def var i       as int no-undo.
def var j       as int no-undo.
def var ldtfrom as date no-undo.
def var litime  as int  no-undo.

create ttcli.
ttcli.clitype = "cont2".

find first daycampaign where
           daycampaign.brand = "1" and
           daycampaign.dcevent = "CONTRATO2" no-lock.

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
               msowner.clitype = mobsub.clitype 
      by msowner.tsbeg:
         fsplitts(msowner.tsbeg,
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
 
end.

disp i j .
