
/* ----------------------------------------------------------------------
  MODULE .......: dss_activation
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: susanjee 
  CREATED ......: 21.04.18
  CHANGED ......:
  Version ......: ccbs
----------------------------------------------------------------------- */
{Func/dss_request.i}
{Func/dss_matrix.i}

FUNCTION fDSSCreateRequest RETURNS INTEGER
   (INPUT iiDSSMsSeq       AS INT,
    INPUT iiDSSCustNum     AS INT,
    INPUT icDSSBundleId    AS CHAR,
    INPUT icDSSReqSource   AS CHAR,
    INPUT iiDSSMainRequest AS INT,
    INPUT iiDSSActStamp    AS DEC,
    INPUT icErrorMsg       AS CHAR,
    OUTPUT ocResult        AS CHAR):  

   DEF VAR liRequest   AS INT  NO-UNDO INITIAL 0.    
   DEF VAR lcBundleId  AS CHAR NO-UNDO INITIAL "". 
   DEF VAR ioDSSMsSeq  AS INT  NO-UNDO INITIAL 0.
   DEF VAR deoDSSLimit AS DEC  NO-UNDO INITIAL 0. 
   DEF VAR coBundleId  AS CHAR NO-UNDO INITIAL "". 

   DEFINE BUFFER lbMobSub      FOR MobSub.
   DEFINE BUFFER bTerMsRequest FOR MsRequest.
   DEFINE BUFFER bMsRequest    FOR MsRequest.

   liRequest = fDSSRequest(iiDSSMsSeq,
                           iiDSSCustNum,
                           "CREATE",
                           "",
                           icDSSBundleId,
                           Func.Common:mSecOffSet(iiDSSActStamp,180),
                           icDSSReqSource,
                           "",
                           TRUE, /* create fees */
                           iiDSSMainRequest,
                           FALSE,
                           OUTPUT ocResult).

   IF liRequest = 0 THEN
      Func.Common:mWriteMemo("MobSub",
                             STRING(iiDSSMsSeq),
                             iiDSSCustNum,
                             (icDSSBundleId + " " + icErrorMsg),
                             ocResult). 
   ELSE DO:
      /* Link MainRequest Id to current created DSS group request */
      /* so it can be skipped while processing percontr.p         */
      IF icDSSReqSource EQ {&REQUEST_SOURCE_STC} THEN DO:
         FIND FIRST bMsRequest EXCLUSIVE-LOCK WHERE
                    bMsRequest.MsRequest EQ liRequest NO-ERROR.
         IF AVAIL bMsRequest THEN DO:
            bMsRequest.ReqIParam2 = iiDSSMainRequest.
            RELEASE bMsRequest.
         END.   
      END.

      fGetDSSMsSeqLimit(iiDSSCustNum,
                        Func.Common:mMakeTS(),
                        OUTPUT ioDSSMsSeq,
                        OUTPUT deoDSSLimit,
                        OUTPUT coBundleId).
      
      IF icDSSBundleId EQ {&DSS4} AND 
         coBundleId    EQ {&DSS2} THEN DO:

         FIND FIRST bTerMsRequest NO-LOCK USE-INDEX CustNum WHERE
                    bTerMsRequest.Brand      EQ Syst.Var:gcBrand AND
                    bTerMsRequest.ReqType    EQ 83               AND
                    bTerMsRequest.Custnum    EQ iiDSSCustNum     AND
                    bTerMsRequest.ReqCParam3 EQ coBundleId       AND
                    bTerMsRequest.ReqCParam1 EQ "DELETE"         AND
                    LOOKUP(STRING(bTerMsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") EQ 0  NO-ERROR.

         IF NOT AVAIL bTerMsRequest THEN DO:
          
            FIND FIRST lbMobSub NO-LOCK WHERE 
                       lbMobSub.MsSeq EQ ioDSSMsSeq NO-ERROR.

            IF AVAIL lbMobSub THEN            
               RUN pUpdateDSSNetwork(INPUT lbMobsub.MsSeq,
                                     INPUT lbMobsub.CLI,
                                     INPUT lbMobsub.CustNum,
                                     INPUT "DELETE",
                                     INPUT "",      /* Optional param list */
                                     INPUT iiDSSMainRequest,
                                     INPUT Func.Common:mSecOffSet(iiDSSActStamp,90),
                                     INPUT icDSSReqSource,
                                     INPUT coBundleId).         
         END.
          
      END.

   END.

   RETURN liRequest.

END FUNCTION.            

FUNCTION fDSSAddRequest RETURNS LOGICAL
   (INPUT iiDSSMsSeq    AS INT,
    INPUT icDSSBundleId AS CHAR,
    INPUT iiMsRequest   AS INT,
    INPUT icMsReqSource AS CHAR,
    INPUT ideActStamp   AS DEC): 
   
   DEFINE BUFFER lbMobSub FOR MobSub. 

   FIND FIRST lbMobSub NO-LOCK WHERE 
              lbMobSub.MsSeq EQ iiDSSMsSeq NO-ERROR.

   IF NOT AVAIL lbMobSub THEN 
      RETURN FALSE.

   IF ideActStamp EQ 0 OR 
      ideActStamp EQ ? THEN 
      ideActStamp = Func.Common:mSecOffSet(Func.Common:mMakeTS(),180). /* 3 mins delay */

   RUN pUpdateDSSNetwork(INPUT lbMobsub.MsSeq,
                         INPUT lbMobsub.CLI,
                         INPUT lbMobSub.CustNum,
                         INPUT "ADD",
                         INPUT "",     
                         INPUT iiMsRequest,
                         INPUT ideActStamp, 
                         INPUT icMsReqSource,
                         INPUT icDSSBundleId).

   RETURN TRUE.

END FUNCTION. 

FUNCTION fDSSCreateDSS2ToDSS4 RETURNS LOGICAL
   (INPUT iiDSSMsSeq    AS INT,
    INPUT iiMsRequest   AS INT,
    INPUT icMsReqSource AS CHAR,
    INPUT ideActStamp   AS DEC,
    INPUT icErrorMsg    AS CHAR):

   DEFINE BUFFER bMobSub   FOR MobSub.
   DEFINE BUFFER lbMobSub  FOR MobSub.
   DEFINE BUFFER bELMobSub FOR MobSub.

   DEF VAR liDSSMsSeq AS INT  NO-UNDO. 
   DEF VAR lcError    AS CHAR NO-UNDO. 
   DEF VAR liRequest  AS INT  NO-UNDO. 

   FIND FIRST lbMobSub NO-LOCK WHERE 
              lbMobSub.MsSeq EQ iiDSSMsSeq NO-ERROR.

   IF NOT AVAIL lbMobSub THEN 
      RETURN FALSE.
  
   IF ideActStamp EQ 0 OR
      ideActStamp EQ ? THEN
      ideActStamp = Func.Common:mSecOffSet(Func.Common:mMakeTS(),180). /* 3 mins delay */
   
   IF fCLITypeIsMainLine(lbMobSub.CLIType) THEN DO:
      FIND FIRST bELMobSub NO-LOCK WHERE
                 bELMobSub.Brand      EQ Syst.Var:gcBrand AND
                 bELMobSub.MultiSimId EQ lbMobSub.MsSeq   NO-ERROR.

      IF NOT AVAIL bELMobSub THEN 
         RETURN FALSE.

      IF NOT fIsDSSActivationAllowed(bELMobSub.CustNum,
                                     bELMobSub.MsSeq,
                                     ideActStamp,
                                     {&DSS4},
                                     OUTPUT liDSSMsSeq,
                                     OUTPUT lcError) THEN 
         RETURN FALSE.
   END.
   ELSE IF fCLITypeIsExtraLine(lbMobSub.CLIType) THEN DO:
      IF NOT fIsDSSActivationAllowed(lbMobSub.CustNum,
                                     lbMobSub.MsSeq,
                                     ideActStamp,
                                     {&DSS4},
                                     OUTPUT liDSSMsSeq,
                                     OUTPUT lcError) THEN 
      RETURN FALSE.
   END.

   FIND FIRST bMobSub NO-LOCK WHERE
              bMobSub.MsSeq EQ liDSSMsSeq NO-ERROR.

   IF NOT AVAIL bMobSub THEN 
      RETURN FALSE.

   liRequest = fDSSCreateRequest(bMobSub.MsSeq,
                                 bMobSub.CustNum,
                                 {&DSS4},
                                 icMsReqSource,
                                 iiMsRequest,
                                 ideActStamp,
                                 icErrorMsg, 
                                 OUTPUT lcError).
   IF liRequest GT 0 THEN
      RETURN TRUE.
   ELSE 
      RETURN FALSE.

END FUNCTION.

FUNCTION fDSSAddExtralineGroup RETURNS LOGICAL
   (INPUT iiDSSMsSeq    AS INT,
    INPUT icDSSBundleId AS CHAR,
    INPUT iiMsRequest   AS INT,
    INPUT icMsReqSource AS CHAR,
    INPUT ideActStamp   AS DEC):

   DEFINE BUFFER lbMobSub  FOR MobSub.
   DEFINE BUFFER bELMobSub FOR MobSub. 

   FIND FIRST lbMobSub NO-LOCK WHERE 
              lbMobSub.MsSeq EQ iiDSSMsSeq NO-ERROR.

   IF NOT AVAIL lbMobSub THEN 
      RETURN FALSE.
  
   IF ideActStamp EQ 0 OR
      ideActStamp EQ ? THEN
      ideActStamp = Func.Common:mSecOffSet(Func.Common:mMakeTS(),180). /* 3 mins delay */

   fDSSAddRequest(lbMobsub.MsSeq,
                  icDSSBundleId,
                  iiMsRequest,
                  icMsReqSource,
                  ideActStamp).

   /* Extraline Business Functionality                                            */
   /* Rule 1: In case of adding any Mainline subscription to existing DSS group,  */
   /*         then add its associated Extralines to the group.                    */
   /* Rule 2: In case of adding any Extraline subscription to existing DSS group, */
   /*         then its associated mainline DSS add request call has to be         */
   /*         done only once - when its first Extraline subscription is added     */
   IF fCLITypeIsMainLine(lbMobSub.CLIType) THEN DO:
       FOR EACH bELMobSub NO-LOCK WHERE
                bELMobSub.Brand      EQ Syst.Var:gcBrand AND
                bELMobSub.MultiSimId EQ lbMobSub.MsSeq:
          fDSSAddRequest(bELMobSub.MsSeq,
                         icDSSBundleId,
                         iiMsRequest,
                         icMsReqSource,
                         ideActStamp).
       END.
   END.
   ELSE IF fCLITypeIsExtraLine(lbMobSub.CLIType)             AND
           fExtraLineCountForMainLine(lbMobSub.MultiSimId,
                                      lbMobSub.CustNum) EQ 1 THEN DO:
      fDSSAddRequest(lbMobSub.MultiSimId,
                     icDSSBundleId,
                     iiMsRequest,
                     icMsReqSource,
                     ideActStamp).
   END.

   RETURN TRUE.   

END FUNCTION.           
