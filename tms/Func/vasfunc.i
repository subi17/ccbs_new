/* ----------------------------------------------------------------------
  MODULE .......: vasfunc.i
  TASK .........: Functions for handling Value Added Service related functions
                  Reference: YPRO-project
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: 14.6.2017
  CHANGED ......:
  ------------------------------------------------------------------------*/
 
{Syst/tmsconst.i}
&IF "{&VASFUNC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE VASFUNC_I YES
{Func/profunc.i}

/*SVA of Yoigo PRO*/
/*This function provides a dirty solution.
Later in YPRO project we will consider if it is reason to make 
1) tmsparam to configure the SVAs
2) own table to configure SVA needs
3) make wider solution safe mapping table for the services */
FUNCTION fIsSVA RETURNS LOGICAL
   (INPUT icService AS CHAR,
    OUTPUT oiParams AS INT):
   oiParams = 0. 
   IF icService EQ "FAXTOEMAIL" THEN DO:
      oiParams = 2.
      RETURN TRUE.
   END.
   ELSE IF icService EQ "OFFICE365" THEN DO:
      oiParams = 1.
      RETURN TRUE.
   END.
   ELSE IF icService EQ "SAGEONE" THEN DO:
      RETURN TRUE.
   END.
   ELSE IF icService EQ "IPFIJA" THEN DO:
      RETURN TRUE.
   END.
   ELSE IF icService EQ "Centralita" THEN DO:
      RETURN TRUE.
   END.

   RETURN FALSE.
END.

FUNCTION fTerminateSVAs RETURNS LOGICAL
   (INPUT iiMsseq AS INT,
    INPUT ilWaitConfirm AS LOG):
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR liAmt AS INT  NO-UNDO.
   
   DEF BUFFER MSRequest FOR MSRequest.
   DEF BUFFER bMsRequest FOR MSRequest.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq EQ iiMsSeq AND
            MsRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus EQ {&REQUEST_STATUS_DONE}
            BREAK BY MsRequest.ReqCparam3
            BY MsRequest.actstamp DESC:
      IF NOT FIRST-OF(MsRequest.ReqCparam3) THEN NEXT.
      IF NOT fisSVA(msRequest.reqcparam3, OUTPUT liAmt) THEN NEXT.
      IF CAN-FIND(FIRST bMsRequest NO-LOCK WHERE
                        bMsRequest.MsSeq EQ iiMsSeq AND
                        bMsRequest.ReqType EQ {&REQTYPE_CONTRACT_TERMINATION} AND
                        bMsRequest.ReqStatus EQ {&REQUEST_STATUS_DONE} AND
                        bMsrequest.reqcparam3 EQ msrequest.reqcparam3 AND
                        bMsrequest.actstamp > Msrequest.actstamp) THEN NEXT.
      fMakeProActRequest(iiMsSeq,
                         msrequest.reqcparam3,
                         Func.Common:mSecOffSet(Func.Common:mMakeTS(),60),
                         STRING(ilWaitConfirm),
                         "",
                         "term",
                         lcErr).
   END.

   RETURN TRUE.
END.

&ENDIF


