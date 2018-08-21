/* ----------------------------------------------------------------------
  MODULE .......: dss_deactivation
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: susanjee
  CREATED ......: 10.06.18
  CHANGED ......:
  Version ......: ccbs
----------------------------------------------------------------------- */
{Func/dss_request.i}
{Func/dss_matrix.i}

FUNCTION fUpdateDSSNewtorkForExtraLine RETURNS LOGICAL
   (INPUT iiMsSeq        AS INT,
    INPUT icCLIType      AS CHAR,
    INPUT iiMultiSimId   AS INT,
    INPUT iiMsRequest    AS INT,
    INPUT ideActStamp    AS DEC,
    INPUT lcBundleId     AS CHAR):

   DEF VAR lcDSSBundleId AS CHAR NO-UNDO. 

   DEFINE BUFFER lbMLMobSub FOR MobSub.
   DEFINE BUFFER lbELMobSub FOR MobSub.

   IF NOT fCheckActiveExtraLinePair(iiMsSeq,
                                    icCLIType,
                                    OUTPUT lcDSSBundleId) THEN
      RETURN FALSE.

   IF fCLITypeIsExtraLine(icCLIType) THEN DO:

      FIND FIRST lbMLMobSub NO-LOCK WHERE
                 lbMLMobSub.MsSeq      = iiMultiSimId        AND
                (lbMLMobSub.MsStatus   = {&MSSTATUS_ACTIVE}  OR
                 lbMLMobSub.MsStatus   = {&MSSTATUS_BARRED}) NO-ERROR.
      IF AVAIL lbMLMobSub THEN DO:
         IF fExtraLineCountForMainLine(lbMLMobsub.MsSeq,
                                       lbMLMobsub.CustNum) EQ 1 THEN
            RUN pUpdateDSSNetwork(INPUT lbMLMobsub.MsSeq,
                                  INPUT lbMLMobsub.CLI,
                                  INPUT lbMLMobsub.CustNum,
                                  INPUT "REMOVE",
                                  INPUT "",        /* Optional param list */
                                  INPUT iiMsRequest,
                                  INPUT ideActStamp,
                                  INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                  INPUT lcBundleId).
      END.
   END.
   ELSE IF fCLITypeIsMainLine(icCLIType) THEN DO:

      FOR EACH lbELMobSub NO-LOCK WHERE
               lbELMobSub.Brand        EQ Syst.Var:gcBrand      AND
               lbELMobSub.MultiSimId   EQ iiMsSeq               AND
               lbELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:

         RUN pUpdateDSSNetwork(INPUT lbELMobsub.MsSeq,
                               INPUT lbELMobsub.CLI,
                               INPUT lbELMobsub.CustNum,
                               INPUT "REMOVE",
                               INPUT "",        /* Optional param list */
                               INPUT iiMsRequest,
                               INPUT ideActStamp,
                               INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                               INPUT lcBundleId).

      END.

   END.

   RETURN TRUE.

END FUNCTION.

FUNCTION fDSSRemoveRequest RETURNS LOGICAL
   (INPUT iiDSSMsSeq    AS INT,
    INPUT icDSSBundleId AS CHAR,
    INPUT iiMsRequest   AS INT,
    INPUT icMsReqSource AS CHAR,
    INPUT ideActStamp   AS DEC,
    INPUT llgELMatrix   AS LOG):

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
                         INPUT "REMOVE",
                         INPUT "",
                         INPUT iiMsRequest,
                         INPUT ideActStamp,
                         INPUT icMsReqSource,
                         INPUT icDSSBundleId).
   IF llgELMatrix THEN 
      fUpdateDSSNewtorkForExtraLine(lbMobSub.MsSeq,
                                    lbMobSub.CLIType,
                                    lbMobSub.MultiSimId,
                                    iiMsRequest,
                                    ideActStamp,
                                    icDSSBundleId).

   RETURN TRUE.

END FUNCTION.

