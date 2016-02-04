

{Syst/commali.i}

/* CC admin tool */


    ADMIN-CC:
    REPEAT:

      ASSIGN
        ufk       = 0
        ufk[2]    = 754 /* BILLING ITEMS */
        ufk[5]    = 401 /* EVENT MANAG */
        ufk[8]    = 8
        ehto      = 0.
       RUN Syst/ufkey .
       
       IF toimi = 2 THEN DO:
      
          DEFINE VARIABLE lcBIGroup AS CHARACTER NO-UNDO. 
          FIND TMSParam WHERE TMSParam.Brand = "1" AND
                              TMSParam.ParamGroup = "CCAdminTool" AND
                              TMSParam.ParamCode = "BIGroup" NO-LOCK NO-ERROR.
          IF AVAIL TMSParam THEN DO:
            lcBIGroup = TMSParam.CharVal.
            RUN Mc/nntuyp_run (lcBIGroup, "update-mode-cc").
          END.

       END.

       IF toimi = 5 THEN DO:
          DEFINE VARIABLE liFMGroup AS INTEGER NO-UNDO. 
          FIND TMSParam WHERE TMSParam.Brand = "1" AND
                              TMSParam.ParamGroup = "CCAdminTool" AND
                              TMSParam.ParamCode = "FMGroup" NO-LOCK NO-ERROR.

          IF AVAIL TMSParam THEN DO:
             liFMGroup = TMSParam.IntVal.
             RUN Mc/bevent_run (liFMGroup).
          END.

       END.

       IF toimi = 8 THEN LEAVE.

    END. /* ADMIN-CC */


