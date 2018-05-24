
/* Directories */

DEF VAR lcIncomingDirectory  AS CHAR NO-UNDO INITIAL "/store/riftp/yoicard/incoming/yoicardcc/". 

/* Incoming files directory */
FIND FIRST ordercanal.TMSParam EXCLUSIVE-LOCK WHERE
           ordercanal.TMSParam.Brand      EQ "1"        AND
           ordercanal.TMSParam.ParamGroup EQ "yoicard"  AND
           ordercanal.TMSParam.ParamCode  EQ "IncDirCC"                
     USE-INDEX ParamGroup NO-ERROR.
IF NOT AVAILABLE ordercanal.TMSParam THEN DO:
   CREATE ordercanal.TMSParam.
   ASSIGN
      ordercanal.TMSParam.Brand      = "1"                                   
      ordercanal.TMSParam.ParamGroup = "yoicard"                           
      ordercanal.TMSParam.ParamCode  = "IncDirCC"                 
      ordercanal.TMSParam.ParamName  = "Incoming directory for yoicard status file"              
      ordercanal.TMSParam.ParamType  = "C"                                   
      ordercanal.TMSParam.OnLine     = NO.
END.
ASSIGN
  ordercanal.TMSParam.CharVal = lcIncomingDirectory. 
RELEASE ordercanal.TMSParam. 
