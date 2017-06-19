DEFINE VARIABLE lcProgram    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRunCommand AS CHARACTER NO-UNDO.

FOR EACH FuncRunConfig EXCLUSIVE-LOCK:

   IF INDEX(FuncRunConfig.RunCommand,"xfear") = 0
   THEN NEXT.
   
   ASSIGN
      lcProgram = ENTRY(3, FuncRunConfig.RunCommand, " ")
      lcProgram = ENTRY(1,lcProgram,".").

   lcRunCommand = "pike -C /apps/yoigo/tms mbatch -- " +
                  lcProgram +
                  ( IF INDEX(FuncRunConfig.RunCommand,"#HOST") > 0
                    THEN " common ordercanal mobile star fraudcdr counter mcdr mcdrdtl prepcdr roamcdr"
                    ELSE " all" ) +
                  " -param #PARAM" +
                  " -clientlog /scratch/log/funcrun/" +
                  lcProgram + ".log tenant=yoigo umask=0000 " +
                  ( IF INDEX(FuncRunConfig.RunCommand,"#HOST") > 0
                    THEN "alt=common "
                    ELSE "" )
                  + "&".
   
   FuncRunConfig.RunCommand = lcRunCommand.
   
END. 
