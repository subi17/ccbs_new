

{Syst/commali.i}

/* CC admin tool */


    ADMIN-CC:
    REPEAT:

      ASSIGN
        Syst.Var:ufk       = 0
        Syst.Var:ufk[2]    = 754 /* BILLING ITEMS */
        Syst.Var:ufk[5]    = 401 /* EVENT MANAG */
        Syst.Var:ufk[8]    = 8
        Syst.Var:ehto      = 0.
       RUN Syst/ufkey.p .
       
       IF Syst.Var:toimi = 2 THEN DO:
      
          DEFINE VARIABLE lcBIGroup AS CHARACTER NO-UNDO. 
          FIND TMSParam WHERE TMSParam.Brand = "1" AND
                              TMSParam.ParamGroup = "CCAdminTool" AND
                              TMSParam.ParamCode = "BIGroup" NO-LOCK NO-ERROR.
          IF AVAIL TMSParam THEN DO:
            lcBIGroup = TMSParam.CharVal.
            RUN Mc/nntuyp_run.p (lcBIGroup, "update-mode-cc").
          END.

       END.

       IF Syst.Var:toimi = 5 THEN DO:
          DEFINE VARIABLE liFMGroup AS INTEGER NO-UNDO. 
          FIND TMSParam WHERE TMSParam.Brand = "1" AND
                              TMSParam.ParamGroup = "CCAdminTool" AND
                              TMSParam.ParamCode = "FMGroup" NO-LOCK NO-ERROR.

          IF AVAIL TMSParam THEN DO:
             liFMGroup = TMSParam.IntVal.
             RUN Mc/bevent_run.p (liFMGroup).
          END.

       END.

       IF Syst.Var:toimi = 8 THEN LEAVE.

    END. /* ADMIN-CC */


