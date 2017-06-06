/* ----------------------------------------------------------------------
  MODULE .......: profunc.i
  TASK .........: Functions for handling Yoigo PRO related functionality
                  Reference: YPRO-project
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: 24.5.2017
  CHANGED ......:
  ------------------------------------------------------------------------*/
 

&IF "{&YOIGOPROFUNC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE YOIGOPROFINC_I YES

{Func/orderfunc.i}

FUNCTION fIsPro RETURNS LOGICAL
   (icCategory AS CHAR):

   FIND FIRST CustCat NO-LOCK where
              CustCat.Brand EQ Syst.Parameters:gcbrand AND
              CustCat.Category EQ icCategory NO-ERROR.
              
   IF AVAIL CustCat AND Custcat.pro THEN RETURN TRUE.
   RETURN FALSE.
END.

FUNCTION fMakeProAckRequest RETURNS LOGICAL (
   INPUT iiMsSeq AS INT,
   INPUT icContr AS CHAR,
   INPUT idActStamp AS DEC,
   INPUT iiMsRequest AS INT):
   DEF VAR liRequest AS INT NO-UNDO.
   DEF VAR lcError         AS CHAR NO-UNDO.

   liRequest = fPCActionRequest(iiMsSeq,
                                icContr,
                                "act",
                                idActStamp,
                                TRUE, /* fees */
                                {&REQUEST_SOURCE_CONTRACT_ACTIVATION},
                                "",
                                iiMsRequest,
                                FALSE,
                                "",
                                0,
                                0,
                                OUTPUT lcError).
   IF liRequest = 0 THEN
      /* write possible error to a memo */
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                       STRING(MsRequest.MsSeq),
                       MsRequest.Custnum,
                       icContr + " activation failed",
                       lcError).
END.

FUNCTION fProMobileAct RETURN LOGICAL 
   (INPUT iimsseq AS INT,
    INPUT idactstamp AS DEC,
    INPUT iiMsrequest AS INT):
   fMakeProAckRequest(iimsseq,"FLEX_UPSELL_500MB",idactstamp,iiMsrequest).
   fMakeProAckRequest(iimsseq,"VOICE5000",idactstamp,iiMsrequest).
   fMakeProAckRequest(iimsseq,"INT_VOICE100",idactstamp,iiMsrequest).
   fMakeProAckRequest(iimsseq,"SMS5000",idactstamp,iiMsrequest).
END.    

FUNCTION fProFixedAct RETURN LOGICAL
   (INPUT iimsseq AS INT,
    INPUT idactstamp AS DEC,
    INPUT iiMsrequest AS INT):
   fMakeProAckRequest(iimsseq,"FIX_VOICE1000",idactstamp,iiMsrequest).
   fMakeProAckRequest(iimsseq,"INT_FIX_VOICE1000",idactstamp,iiMsrequest).
END.

&ENDIF


