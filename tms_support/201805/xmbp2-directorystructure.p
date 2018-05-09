
/* Directories */
DEF VAR lcBaseDirectory      AS CHAR NO-UNDO INITIAL "/tmp/yoicard/".
DEF VAR lcIncomingDirectory  AS CHAR NO-UNDO INITIAL "/tmp/yoicard/incoming/". 
DEF VAR lcOutgoingDirectory  AS CHAR NO-UNDO INITIAL "/tmp/yoicard/outgoing/". 
DEF VAR lcProcessedDirectory AS CHAR NO-UNDO INITIAL "/tmp/yoicard/processed/".
DEF VAR lcLogsDirectory      AS CHAR NO-UNDO INITIAL "/tmp/yoicard/logs/".
 
/* Base directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "yoicard" AND
           ordercanal.TMSParam.ParamCode  EQ "yoicardBaseDir"               
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "yoicard"                       
      ordercanal.TMSParam.ParamCode  = "yoicardBaseDir"               
      ordercanal.TMSParam.ParamName  = "Base directory for yoicard"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcBaseDirectory. 
RELEASE ordercanal.TMSParam.  

/* Incoming files directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "yoicard" AND
           ordercanal.TMSParam.ParamCode  EQ "yoicardIncomingDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                   
      ordercanal.TMSParam.ParamGroup = "yoicard"                           
      ordercanal.TMSParam.ParamCode  = "yoicardIncomingDir"                 
      ordercanal.TMSParam.ParamName  = "Incoming directory for yoicard"              
      ordercanal.TMSParam.ParamType  = "C"                                   
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcIncomingDirectory. 
RELEASE ordercanal.TMSParam. 

/* Outgoing files directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "yoicard" AND
           ordercanal.TMSParam.ParamCode  EQ "yoicardOutgoingDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                   
      ordercanal.TMSParam.ParamGroup = "yoicard"                           
      ordercanal.TMSParam.ParamCode  = "yoicardOutgoingDir"                 
      ordercanal.TMSParam.ParamName  = "Outgoing directory for yoicard"  
      ordercanal.TMSParam.ParamType  = "C"                                   
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcOutgoingDirectory. 
RELEASE ordercanal.TMSParam. 

/* Processed directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "yoicard" AND
           ordercanal.TMSParam.ParamCode  EQ "yoicardProcessedDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "yoicard"                       
      ordercanal.TMSParam.ParamCode  = "yoicardProcessedDir"                 
      ordercanal.TMSParam.ParamName  = "Processed directory for yoicard"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcProcessedDirectory. 
RELEASE ordercanal.TMSParam. 

/* Logs directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"         AND
           ordercanal.TMSParam.ParamGroup EQ "yoicard" AND
           ordercanal.TMSParam.ParamCode  EQ "yoicardLogsDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "yoicard"                       
      ordercanal.TMSParam.ParamCode  = "yoicardLogsDir"                 
      ordercanal.TMSParam.ParamName  = "Logs directory for yoicard "              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcLogsDirectory. 
RELEASE ordercanal.TMSParam. 