/* YOT-5906.                               */
/* Updating SLGAnalyse for CONTFHNB58_300. */
/* jotorres. 30.07.2018                    */

DEFINE VARIABLE ldInitialDate AS DATE NO-UNDO INITIAL 05/03/18. /* CONTFHNB58_300 creation date */
DEFINE BUFFER bSLGAnalyse FOR SLGAnalyse.

FOR EACH SLGAnalyse NO-LOCK WHERE 
         SLGAnalyse.Brand   EQ Syst.Var:gcBrand AND   
         SLGAnalyse.CliType EQ "CONTFH58_300":
   IF NOT CAN-FIND(FIRST bSLGAnalyse WHERE
                         bSLGAnalyse.CliType            EQ "CONTFHNB58_300"             AND   
                         bSLGAnalyse.BelongTo           EQ SLGAnalyse.BelongTo          AND  
                         bSLGAnalyse.BillCode           EQ SLGAnalyse.BillCode          AND
                         bSLGAnalyse.CCN                EQ SLGAnalyse.CCN               AND
                         bSLGAnalyse.BDest              EQ SLGAnalyse.BDest             AND                                      
                         bSLGAnalyse.ServiceLimitGroup  EQ SLGAnalyse.ServiceLimitGroup AND                         
                         bSLGAnalyse.Brand              EQ SLGAnalyse.Brand) THEN DO:                           
      CREATE bSLGAnalyse.
      BUFFER-COPY SLGAnalyse TO bSLGAnalyse 
      ASSIGN 
         bSLGAnalyse.CliType   = "CONTFHNB58_300"
         bSLGAnalyse.ValidFrom = (IF SLGAnalyse.ValidFrom > ldInitialDate 
                                  THEN SLGAnalyse.ValidFrom 
                                  ELSE 05/03/2018).            
   END.
END.
 
