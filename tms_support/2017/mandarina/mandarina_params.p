/* ----------------------------------------------------------------------
  MODULE .......: mandarina_params.p
  TASK .........: BOB / Creating/updating parameters for MANDARINA LP
  APPLICATION ..: TMS
  AUTHOR .......: jotorres & ilsavola
  CREATED ......: 08/2017
  Version ......: yoigo
---------------------------------------------------------------------- */

/* Creating/updating parameters for Mandarina BOB tool */
/* https://kethor.qvantel.com/browse/MANDLP-8          */

/* Search for the parameters. If they don't exist, create them. */
/* If they exist, then update with the new values.              */

/* Directories */
DEF VAR lcBaseDirectory     AS CHAR NO-UNDO INITIAL "/tmp/mnt/store/riftp/mandarina/".
DEF VAR lcSpoolDirectory    AS CHAR NO-UNDO INITIAL "/tmp/mnt/store/riftp/mandarina/spool/". 
DEF VAR lcIncomingDirectory AS CHAR NO-UNDO INITIAL "/tmp/mnt/store/riftp/mandarina/incoming/". 
DEF VAR lcOutgoingDirectory AS CHAR NO-UNDO INITIAL "/tmp/mnt/store/riftp/mandarina/outgoing/". 
DEF VAR lcLogsDirectory     AS CHAR NO-UNDO INITIAL "/tmp/mnt/store/riftp/mandarina/logs/". 

/* Network delay */
DEF VAR lcDelayNW AS CHAR NO-UNDO INITIAL "5".  /* Delay time in seconds */ 
DEF VAR lcItemsNW AS CHAR NO-UNDO INITIAL "20". /* Number of Items in batch */

/* Base directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      = "1"         AND
           ordercanal.TMSParam.ParamGroup = "Mandarina" AND
           ordercanal.TMSParam.ParamCode  = "MandarinaBaseDir"               
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "Mandarina"                       
      ordercanal.TMSParam.ParamCode  = "MandarinaBaseDir"               
      ordercanal.TMSParam.ParamName  = "Base directory for Mandarina LP"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcBaseDirectory. 
RELEASE ordercanal.TMSParam. 

/* Spool directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      = "1"         AND
           ordercanal.TMSParam.ParamGroup = "Mandarina" AND
           ordercanal.TMSParam.ParamCode  = "MandarinaSpoolDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                
      ordercanal.TMSParam.ParamGroup = "Mandarina"                       
      ordercanal.TMSParam.ParamCode  = "MandarinaSpoolDir"                 
      ordercanal.TMSParam.ParamName  = "Spool directory for Mandarina LP"              
      ordercanal.TMSParam.ParamType  = "C"                                
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcSpoolDirectory. 
RELEASE ordercanal.TMSParam. 

/* Incoming files directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      = "1"         AND
           ordercanal.TMSParam.ParamGroup = "Mandarina" AND
           ordercanal.TMSParam.ParamCode  = "MandarinaIncomingDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                   
      ordercanal.TMSParam.ParamGroup = "Mandarina"                           
      ordercanal.TMSParam.ParamCode  = "MandarinaIncomingDir"                 
      ordercanal.TMSParam.ParamName  = "Incoming directory for Mandarina LP"              
      ordercanal.TMSParam.ParamType  = "C"                                   
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcIncomingDirectory. 
RELEASE ordercanal.TMSParam. 

/* Outgoing files directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      = "1"         AND
           ordercanal.TMSParam.ParamGroup = "Mandarina" AND
           ordercanal.TMSParam.ParamCode  = "MandarinaOutgoingDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                   
      ordercanal.TMSParam.ParamGroup = "Mandarina"                           
      ordercanal.TMSParam.ParamCode  = "MandarinaOutgoingDir"                 
      ordercanal.TMSParam.ParamName  = "Outgoing directory for Mandarina LP"  
      ordercanal.TMSParam.ParamType  = "C"                                   
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcOutgoingDirectory. 
RELEASE ordercanal.TMSParam. 

/* Logs directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      = "1"         AND
           ordercanal.TMSParam.ParamGroup = "Mandarina" AND
           ordercanal.TMSParam.ParamCode  = "MandarinaLogsDir"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "Mandarina"                       
      ordercanal.TMSParam.ParamCode  = "MandarinaLogsDir"                 
      ordercanal.TMSParam.ParamName  = "Logs directory for Mandarina LP"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcLogsDirectory. 
RELEASE ordercanal.TMSParam. 

/* NetWorkDelay, in seconds */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      = "1"         AND
           ordercanal.TMSParam.ParamGroup = "Mandarina" AND
           ordercanal.TMSParam.ParamCode  = "NetWorkDelay"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "Mandarina"                       
      ordercanal.TMSParam.ParamCode  = "NetWorkDelay"                 
      ordercanal.TMSParam.ParamName  = "Network delay, in seconds"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcDelayNW. 
RELEASE ordercanal.TMSParam. 

/* Number of Network Batch Items */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      = "1"         AND
           ordercanal.TMSParam.ParamGroup = "Mandarina" AND
           ordercanal.TMSParam.ParamCode  = "NetWorkBatchItems"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                               
      ordercanal.TMSParam.ParamGroup = "Mandarina"                       
      ordercanal.TMSParam.ParamCode  = "NetWorkBatchItems"                 
      ordercanal.TMSParam.ParamName  = "Network Batch Items"              
      ordercanal.TMSParam.ParamType  = "C"                               
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcItemsNW. 
RELEASE ordercanal.TMSParam. 
