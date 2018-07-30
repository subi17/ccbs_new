/* YOT-5906.                               */
/* Updating SLGAnalyse for CONTFHNB58_300. */
/* It must be equivalent to CONTFH58_300.  */
/* jotorres. 30.07.2018                    */

DEFINE VARIABLE ldInitialDate AS DATE NO-UNDO INITIAL 05/03/18. /* CONTFHNB58_300 creation date */
DEFINE BUFFER bSLGAnalyse FOR SLGAnalyse.

FOR EACH SLGAnalyse WHERE 
         SLGAnalyse.CliType = "CONTFHNB58_300":
  DELETE SLGAnalyse.
END.

FOR EACH SLGAnalyse NO-LOCK WHERE 
         SLGAnalyse.CliType = "CONTFH58_300":            
   CREATE bSLGAnalyse.
   BUFFER-COPY SLGAnalyse TO bSLGAnalyse 
   ASSIGN 
      bSLGAnalyse.CliType = "CONTFHNB58_300"
      bSLGAnalyse.ValidFrom = (IF SLGAnalyse.ValidFrom > ldInitialDate 
                               THEN SLGAnalyse.ValidFrom 
                               ELSE 05/03/2018).            
END. 
