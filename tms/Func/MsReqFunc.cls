 
 /*------------------------------------------------------------------------
    File        : MsReqFunc
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 11:50:19 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Func.MsReqFunc: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PRIVATE MsReqFunc():
	END CONSTRUCTOR.

   METHOD PUBLIC STATIC VOID mRejectOrigReq
      ( liOrigRequest AS INTEGER,
        liSubReqType  AS INTEGER ):

      DEFINE BUFFER MsRequest FOR MsRequest.

      FIND MsRequest NO-LOCK WHERE MsRequest.MsRequest = liOrigRequest NO-ERROR.
      
      IF NOT AVAILABLE MsRequest OR MsRequest.ReqStatus EQ 3
      THEN RETURN.
      
      DO TRANSACTION:
         FIND CURRENT MsRequest EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         
         IF LOCKED(MsRequest)
         THEN RETURN.
         
         ASSIGN
            MsRequest.UpdateStamp = Func.Common:mMakeTS()
            MsRequest.ReqStatus   = {&REQUEST_STATUS_REJECTED}
            MsRequest.Memo        = MsRequest.Memo + 
                                      (IF MsRequest.Memo > ""
                                       THEN ", " 
                                       ELSE "") +
                                      "Subrequest " + 
                                      STRING(liSubReqType) + " failed".
      END.

   END METHOD.

END CLASS.