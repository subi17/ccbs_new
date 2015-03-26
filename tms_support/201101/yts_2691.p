{date.i}
DEFINE VARIABLE lcProcesses AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcess AS CHARACTER NO-UNDO. 

lcProcesses = "00530141110127081300291,00502011110125081701463,00502011110125132415602".
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

do i = 1 to num-entries(lcProcesses) with frame a:
   find mnpprocess where
        mnpprocess.portrequest = entry(i,lcProcesses) EXCLUSIVE-LOCK.
    disp mnpprocess.portrequest mnpprocess.statuscode.

    assign
      mnpprocess.statusreason = "CANC_TECNI"
      mnpprocess.updatets = fMakeTS()
      mnpprocess.statuscode = 7.
end.  
