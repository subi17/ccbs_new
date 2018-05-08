
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

   DEFINE BUFFER lbMobSub FOR MobSub.

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

      fGetDSSMsSeqLimit(iiDSSCustNum,
                        Func.Common:mMakeTS(),
                        OUTPUT ioDSSMsSeq,
                        OUTPUT deoDSSLimit,
                        OUTPUT coBundleId).
      
      IF icDSSBundleId EQ {&DSS4} AND 
         coBundleId    EQ {&DSS2} THEN DO:
          
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

   RETURN liRequest.

END FUNCTION.            

FUNCTION fDSSAddRequest RETURNS LOGICAL
   (INPUT iiDSSMsSeq    AS INT,
    INPUT icDSSBundleId AS CHAR,
    INPUT iiMsRequest   AS INT,
    INPUT icMsReqSource AS CHAR): 
   
   DEFINE BUFFER lbMobSub FOR MobSub. 

   FIND FIRST lbMobSub NO-LOCK WHERE 
              lbMobSub.MsSeq EQ iiDSSMsSeq NO-ERROR.

   IF NOT AVAIL lbMobSub THEN 
      RETURN FALSE.

   RUN pUpdateDSSNetwork(INPUT lbMobsub.MsSeq,
                         INPUT lbMobsub.CLI,
                         INPUT lbMobSub.CustNum,
                         INPUT "ADD",
                         INPUT "",                                                /* Optional param list */
                         INPUT iiMsRequest,
                         INPUT Func.Common:mSecOffSet(Func.Common:mMakeTS(),180), /* 3 mins delay */
                         INPUT icMsReqSource,
                         INPUT icDSSBundleId).

   RETURN TRUE.

END FUNCTION.            
