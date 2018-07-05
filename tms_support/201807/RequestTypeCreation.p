&GLOBAL-DEFINE REQTYPE_INSTALL_ADDRESS_UPDATE 99

FIND FIRST RequestType WHERE 
           RequestType.Brand = "1" AND 
           RequestType.ReqType = 99 
           NO-LOCK NO-ERROR.
IF NOT AVAILABLE RequestType THEN 
DO:  
    CREATE RequestType.
    ASSIGN
        RequestType.Brand        = "1"
        RequestType.Queue        = 3
        RequestType.ReqType      = 99
        RequestType.InUse        = TRUE
        RequestType.ReqName      = "Installation Address Update"        
        RequestType.Program      = ""      
        RequestType.UserCode     = "InstAddrUpdate"       
        RequestType.LogOn        = FALSE        
        RequestType.LogFile      = ""      
        RequestType.LogEntry     = ""      
        RequestType.LogClear     = FALSE  
        RequestType.LogThreshold = 0.
END.

FIND FIRST RequestStatus WHERE 
           RequestStatus.Brand = "1" AND 
           RequestStatus.ReqType = 99 AND 
           RequestStatus.ReqStatus = 0 
           NO-LOCK NO-ERROR.
IF NOT AVAILABLE RequestStatus THEN 
DO:
    CREATE RequestStatus.
    ASSIGN
        RequestStatus.Brand        = "1"
        RequestStatus.ReqType      = RequestType.ReqType
        RequestStatus.ReqStat      = 0
        RequestStatus.InUse        = TRUE
        RequestStatus.Program      = "Mc/installaddr_update.p"        
        RequestStatus.LogOn        = FALSE           
        RequestStatus.LogFile      = ""      
        RequestStatus.LogEntry     = ""      
        RequestStatus.LogClear     = FALSE       
        RequestStatus.LogThreshold = 0.
END.    

function fCreateRequestParam return logical 
   (INPUT icreqtype AS INTEGER,
    input icfieldname as char,
    input icusage as CHARACTER,
    INPUT icDispParam AS LOGICAL):
          
      find first RequestParam WHERE
                 RequestParam.Brand      = "1" AND
                 RequestParam.ReqType    = icreqtype  AND
                 RequestParam.ParamField = icfieldname 
                 exclusive-lock  no-wait no-error.
      if not AVAILABLE RequestParam then DO:
         CREATE RequestParam.     
         ASSIGN 
            RequestParam.Brand       = "1" 
            RequestParam.ReqType     = icreqtype
            RequestParam.ParamField  = icfieldname
            RequestParam.Usage       = icusage   
            RequestParam.Description = ""
            RequestParam.DispParam   = icDispParam.
      END.
      ELSE
         ASSIGN 
            RequestParam.Brand       = "1" 
            RequestParam.ReqType     = icreqtype
            RequestParam.ParamField  = icfieldname
            RequestParam.Usage       = icusage  
            RequestParam.Description = ""
            RequestParam.DispParam   = icDispParam.
end function.

fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ActStamp","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"Brand","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"MsSeq","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"MsRequest","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"CustNum","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqType","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"UserCode","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqStatus","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"CreateFees","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"CreStamp","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"SendSMS","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"CLI","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam1","SalesManId",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam2","Amendment Type",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam6","Change Reason",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam3","Amendment Value",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam4","Current Details",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam5","Contract ID",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqIParam1","Order ID",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqSource","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"OrigRequest","",yes).
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"OrigRequest","",yes).
