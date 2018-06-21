{Syst/tmsconst.i}

FIND FIRST RequestType WHERE 
           RequestType.reqtype EQ {&REQTYPE_INSTALL_ADDRESS_UPDATE} NO-ERROR.
IF NOT AVAIL requesttype THEN DO:
   CREATE Requesttype.
   ASSIGN
      RequestType.Brand    = "1"
      RequestType.Queue    = 11
      RequestType.ReqType  = {&REQTYPE_INSTALL_ADDRESS_UPDATE}
      RequestType.InUse    = TRUE
      RequestType.ReqName  = "Installation Address Update"
      RequestType.Program  = ""
      RequestType.UserCode = "InstAddrUpdate".
END.

FIND FIRST RequestStatus WHERE
           requeststatus.brand EQ "1" AND
           requestStatus.reqtype EQ {&REQTYPE_INSTALL_ADDRESS_UPDATE} AND
           requestStatus.reqstatus EQ 0 NO-ERROR.
IF NOT AVAIL requeststatus THEN DO:
   CREATE Requeststatus.
   ASSIGN
      RequestStatus.Brand     = "1"
      RequestStatus.ReqType   = {&REQTYPE_INSTALL_ADDRESS_UPDATE}
      RequestStatus.reqstatus = 0
      RequestStatus.Program   = "Mc/installaddr_update.p".
END.

FUNCTION fCreateRequestParam RETURNS LOGICAL 
   (INPUT iiReqType AS INT,
    INPUT icParamField AS CHAR,
    INPUT icUsage AS CHAR,
    INPUT icDispParam AS CHAR):
        
   IF CAN-FIND(FIRST RequestParam WHERE
                     RequestParam.Brand   = Syst.Var:gcBrand AND
                     RequestParam.ReqType = iiReqType) THEN DO:
      MESSAGE "Parameter has already been defined for this type"
         VIEW-AS ALERT-BOX ERROR.
      NEXT.
   END.
   ELSE DO:
      CREATE RequestParam.
      ASSIGN 
         RequestParam.Brand = Syst.Var:gcBrand
         RequestParam.ReqType = iiReqType
         RequestParam.ParamField = icParamField
         RequestParam.Usage = icUsage
         RequestParam.DispParam = icDispParam.
   END.                                     
END FUNCTION.

fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ActStamp","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"Brand","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"MsSeq","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"MsRequest","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"CustNum","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqType","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"UserCode","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqStatus","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"CreateFees","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"CreStamp","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"SendSMS","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"CLI","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam1","Amendament Type","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam2","Current Details","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam6","Remaining Current Details","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam3","New Details","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam4","Complete NewDetails","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqCParam5","Contract ID","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqIParam1","Order ID","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"ReqSource","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"OrigRequest","","yes").
fCreateRequestParam({&REQTYPE_INSTALL_ADDRESS_UPDATE},"OrigRequest","","yes").







