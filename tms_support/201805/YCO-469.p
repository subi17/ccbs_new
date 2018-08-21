/*------------------------------------------------------------------------
YCO-469. /tms_support/201805/YCO-469.p
Creating parameters for Periodical Contracts Bob Tool (percontract_bob.p)
Search for the parameters. If they don't exist, then create them. 
If they exist, then update with the new values.              
-------------------------------------------------------------------------*/
 
/* Directories */
DEF VAR lcBaseDirectory      AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/".
DEF VAR lcSpoolDirectory     AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/spool/". 
DEF VAR lcIncomingDirectory  AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/incoming/". 
DEF VAR lcOutgoingDirectory  AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/outgoing/". 
DEF VAR lcProcessedDirectory AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/processed/".
DEF VAR lcLogsDirectory      AS CHAR NO-UNDO INITIAL "/mnt/store/riftp/PeriodicalContract/logs/".
 
/* Base directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "PerContractBob" AND
           ordercanal.TMSParam.ParamCode  EQ "PerContractBobBaseDir"               
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "PerContractBob"                       
      ordercanal.TMSParam.ParamCode  = "PerContractBobBaseDir"               
      ordercanal.TMSParam.ParamName  = "Base directory for Periodical Contract Bob Tool"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcBaseDirectory. 
RELEASE ordercanal.TMSParam. 

/* Spool directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "PerContractBob" AND
           ordercanal.TMSParam.ParamCode  EQ "PerContractBobSpoolDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                
      ordercanal.TMSParam.ParamGroup = "PerContractBob"                       
      ordercanal.TMSParam.ParamCode  = "PerContractBobSpoolDir"                 
      ordercanal.TMSParam.ParamName  = "Spool directory for Periodical Contract Bob Tool"              
      ordercanal.TMSParam.ParamType  = "C"                                
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcSpoolDirectory. 
RELEASE ordercanal.TMSParam. 

/* Incoming files directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "PerContractBob" AND
           ordercanal.TMSParam.ParamCode  EQ "PerContractBobIncomingDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                   
      ordercanal.TMSParam.ParamGroup = "PerContractBob"                           
      ordercanal.TMSParam.ParamCode  = "PerContractBobIncomingDir"                 
      ordercanal.TMSParam.ParamName  = "Incoming directory for Periodical Contract Bob Tool"              
      ordercanal.TMSParam.ParamType  = "C"                                   
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcIncomingDirectory. 
RELEASE ordercanal.TMSParam. 

/* Outgoing files directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "PerContractBob" AND
           ordercanal.TMSParam.ParamCode  EQ "PerContractBobOutgoingDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                   
      ordercanal.TMSParam.ParamGroup = "PerContractBob"                           
      ordercanal.TMSParam.ParamCode  = "PerContractBobOutgoingDir"                 
      ordercanal.TMSParam.ParamName  = "Outgoing directory for Periodical Contract Bob Tool"  
      ordercanal.TMSParam.ParamType  = "C"                                   
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcOutgoingDirectory. 
RELEASE ordercanal.TMSParam. 

/* Processed directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "PerContractBob" AND
           ordercanal.TMSParam.ParamCode  EQ "PerContractBobProcessedDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "PerContractBob"                       
      ordercanal.TMSParam.ParamCode  = "PerContractBobProcessedDir"                 
      ordercanal.TMSParam.ParamName  = "Processed directory for Periodical Contract Bob Tool"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcProcessedDirectory. 
RELEASE ordercanal.TMSParam. 

/* Logs directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "PerContractBob" AND
           ordercanal.TMSParam.ParamCode  EQ "PerContractBobLogsDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "PerContractBob"                       
      ordercanal.TMSParam.ParamCode  = "PerContractBobLogsDir"                 
      ordercanal.TMSParam.ParamName  = "Logs directory for Periodical Contract Bob Tool"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcLogsDirectory. 
RELEASE ordercanal.TMSParam. 
