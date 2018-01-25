 
/* 
    YDR-2769 - Directories for loading "install address change" files.
 */
 

/* Directories */
DEF VAR lcBaseDirectory      AS CHAR NO-UNDO INITIAL "/data/installaddrchg/".
DEF VAR lcIncomingDirectory  AS CHAR NO-UNDO INITIAL "/data/installaddrchg/incoming/". 
DEF VAR lcOutgoingDirectory  AS CHAR NO-UNDO INITIAL "/data/installaddrchg/outgoing/". 
DEF VAR lcProcessedDirectory AS CHAR NO-UNDO INITIAL "/data/installaddrchg/processed/".
DEF VAR lcLogsDirectory      AS CHAR NO-UNDO INITIAL "/data/installaddrchg/logs/".
 
/* Base directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "InstallAddrChg" AND
           ordercanal.TMSParam.ParamCode  EQ "InstallAddrChgBaseDir"               
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "InstallAddrChg"                       
      ordercanal.TMSParam.ParamCode  = "InstallAddrChgBaseDir"               
      ordercanal.TMSParam.ParamName  = "Base directory for InstallAddrChg"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcBaseDirectory. 
RELEASE ordercanal.TMSParam.  

/* Incoming files directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "InstallAddrChg" AND
           ordercanal.TMSParam.ParamCode  EQ "InstallAddrChgIncomingDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                   
      ordercanal.TMSParam.ParamGroup = "InstallAddrChg"                           
      ordercanal.TMSParam.ParamCode  = "InstallAddrChgIncomingDir"                 
      ordercanal.TMSParam.ParamName  = "Incoming directory for InstallAddrChg"              
      ordercanal.TMSParam.ParamType  = "C"                                   
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcIncomingDirectory. 
RELEASE ordercanal.TMSParam. 

/* Outgoing files directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "InstallAddrChg" AND
           ordercanal.TMSParam.ParamCode  EQ "InstallAddrChgOutgoingDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                   
      ordercanal.TMSParam.ParamGroup = "InstallAddrChg"                           
      ordercanal.TMSParam.ParamCode  = "InstallAddrChgOutgoingDir"                 
      ordercanal.TMSParam.ParamName  = "Outgoing directory for InstallAddrChg"  
      ordercanal.TMSParam.ParamType  = "C"                                   
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcOutgoingDirectory. 
RELEASE ordercanal.TMSParam. 

/* Processed directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "InstallAddrChg" AND
           ordercanal.TMSParam.ParamCode  EQ "InstallAddrChgProcessedDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "InstallAddrChg"                       
      ordercanal.TMSParam.ParamCode  = "InstallAddrChgProcessedDir"                 
      ordercanal.TMSParam.ParamName  = "Processed directory for InstallAddrChg"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcProcessedDirectory. 
RELEASE ordercanal.TMSParam. 

/* Logs directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "InstallAddrChg" AND
           ordercanal.TMSParam.ParamCode  EQ "InstallAddrChgLogsDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "InstallAddrChg"                       
      ordercanal.TMSParam.ParamCode  = "InstallAddrChgLogsDir"                 
      ordercanal.TMSParam.ParamName  = "Logs directory for InstallAddrChg "              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcLogsDirectory. 
RELEASE ordercanal.TMSParam. 


