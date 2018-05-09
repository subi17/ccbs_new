{Syst/tmsconst.i}

DEF BUFFER bf_ttSLGAnalyse FOR ttSLGAnalyse.
/* input parameter ccn and bdest come later */ 

FUNCTION fSLGAnalyse RETURN LOGICAL
  (INPUT  icClitype            AS CHAR,
   INPUT  icBillItem           AS CHAR,
   INPUT  idtDate              AS DATE,
   OUTPUT ocServiceLimitGroup  AS CHAR).
 
   ocServiceLimitGroup = "".
   
   /* 1. Step - find first permitted servicelimitgroup */ 
   FIND FIRST ttSLGAnalyse WHERE 
              ttSLGAnalyse.Brand       = "1"          AND 
              ttSLGAnalyse.BelongTo    = TRUE         AND 
              ttSLGAnalyse.CliType     = icClitype    AND
              ttSLGAnalyse.BillCode    = icBillItem   AND 
              ttSLGAnalyse.ccn         = 0            AND
              ttSLGAnalyse.Bdest       = ""           AND 
              ttSLGAnalyse.ValidFrom  <= idtDate      AND 
              ttSLGAnalyse.ValidTo    >= idtDate NO-LOCK NO-ERROR.

   IF AVAIL ttSLGAnalyse THEN DO:

      FIND FIRST bf_ttSLGAnalyse WHERE
                 bf_ttSLGAnalyse.Brand       = "1"          AND 
                 bf_ttSLGAnalyse.BelongTo    = FALSE        AND
                 bf_ttSLGAnalyse.CliType     = icClitype    AND
                 bf_ttSLGAnalyse.BillCode    = icBillItem   AND
                 bf_ttSLGAnalyse.ccn         = 0            AND
                 bf_ttSLGAnalyse.Bdest       = ""           AND
                 bf_ttSLGAnalyse.ValidFrom  <= idtDate      AND
                 bf_ttSLGAnalyse.ValidTo    >= idtDate NO-LOCK NO-ERROR.

      IF NOT Avail bf_ttSLGAnalyse 
      THEN ocServiceLimitGroup = ttSLGAnalyse.ServiceLimitGroup.
   END.

END FUNCTION. 

FUNCTION fPacketAnalyse RETURN LOGICAL
  (INPUT  icClitype            AS CHAR,
   INPUT  icBillItem           AS CHAR,
   INPUT  iiCCN                AS INT,
   INPUT  icBdest              AS CHAR,
   INPUT  idtDate              AS DATE,
   OUTPUT ocServiceLimitGroupList  AS CHAR,
   OUTPUT ocSLGATypeList           AS CHAR).
 
   ocServiceLimitGroupList = "".
   ocSLGATypeList = "".
   /* */ 
   FOR EACH  ttSLGAnalyse NO-LOCK WHERE 
             ttSLGAnalyse.Brand       = Syst.Var:gcBrand      AND 
             ttSLGAnalyse.BelongTo    = TRUE         AND 
             ttSLGAnalyse.CliType     = icClitype    AND
             ttSLGAnalyse.BillCode    = icBillItem   AND 
             ttSLGAnalyse.ccn         = iiCCN        AND
            (ttSLGAnalyse.BDest = "*" OR ttSLGAnalyse.Bdest = icBDest) AND 
             ttSLGAnalyse.ValidFrom  <= idtDate      AND 
             ttSLGAnalyse.ValidTo    >= idtDate  USE-INDEX BelongTo :
     
     /* exclude rules are currently not used, needs performance
        refactoring if used again */
     /* 
      IF CAN-FIND(FIRST bNegative NO-LOCK WHERE
                 bNegative.Brand       = Syst.Var:gcBrand      AND 
                 bNegative.BelongTo    = FALSE        AND
                 bNegative.CliType     = icClitype    AND
                 bNegative.BillCode    = icBillItem   AND
                 bNegative.ccn         = iiCCN        AND
                 icBdest MATCHES bNegative.Bdest      AND 
                 bNegative.ValidFrom  <= idtDate      AND
                 bNegative.ValidTo    >= idtDate USE-INDEX BelongTo) THEN NEXT.
      */
      ASSIGN
         ocServiceLimitGroupList = ocServiceLimitGroupList + "," + 
                                   ttSLGAnalyse.ServiceLimitGroup 
         ocSLGATypeList  = ocSLGATypeList + "," + STRING(ttSLGAnalyse.SLGAType).

   END.

   IF ocServiceLimitGroupList NE "" THEN DO:
      ocServiceLimitGroupList = SUBSTRING(ocServiceLimitGroupList,2).
      ocSLGATypeList = SUBSTRING(ocSLGATypeList,2).
   END.

END FUNCTION. 

FUNCTION fCheckTarget RETURNS LOGICAL
(INPUT  iimsseq     AS INT,
 INPUT  iiCustNum   AS INT,
 INPUT  iiDialtype  AS INT,
 INPUT  idCallTS    AS DEC,
 INPUT  icProd      AS CHAR,
 INPUT  icSLGroup   AS CHAR,
 OUTPUT oiMSID      AS INT,
 OUTPUT ocInBdest   AS CHAR,
 OUTPUT ocOutBdest  AS CHAR,
 OUTPUT oiSlseq     AS INT,
 OUTPUT oiInclUnit  AS INT,
 OUTPUT oiInclAmt   AS DEC,
 OUTPUT oiBdestLimit AS INT):

   DEF VAR llServLimit AS LOG NO-UNDO.
   
   llServLimit = FALSE.
   DEF BUFFER mServiceLimit FOR mServiceLimit.

   LOOP:
   FOR EACH ttServiceLimit NO-LOCK WHERE
            ttServiceLimit.GroupCode = icSLGroup:
       IF ttServiceLimit.GroupCode BEGINS {&DSS} THEN
          FIND FIRST mServiceLimit NO-LOCK WHERE
                     mServiceLimit.CustNum  = iiCustnum       AND
                     mServiceLimit.Dialtype = iiDialType      AND
                     mServiceLimit.SLSeq    = ttServiceLimit.SLSeq AND
                     mServiceLimit.EndTS   >= idCallTS        AND
                     mServiceLimit.FromTS  <= idCallTS NO-ERROR.
       ELSE
          FIND FIRST mServiceLimit NO-LOCK WHERE
                     mServiceLimit.MSSeq    = iiMSSeq         AND
                     mServiceLimit.Dialtype = iiDialType      AND
                     mServiceLimit.SLSeq    = ttServiceLimit.SLSeq AND
                     mServiceLimit.EndTS   >= idCallTS        AND
                     mServiceLimit.FromTS  <= idCallTS NO-ERROR.

       IF NOT AVAIL mServiceLimit THEN NEXT.

       FIND FIRST ServiceLimitTarget NO-LOCK WHERE
                  ServiceLimitTarget.SLSeq = mServiceLimit.SLSeq AND
                  ServiceLimitTarget.ServiceLMember = icProd NO-ERROR.
       IF NOT AVAIL ServiceLimitTarget THEN NEXT.

         ASSIGN
            oiMSID      = mServiceLimit.MSID
            ocInBdest   = ServiceLimitTarget.InSideRate
            ocOutBdest  = ServiceLimitTarget.OutSideRate
            oiSlseq     = ServiceLimitTarget.SLSeq
            oiInclUnit  = MserviceLimit.InclUnit
            oiInclAmt   = MServiceLimit.InclAmt
            oiBDestLimit = ttServiceLimit.BDestLimit
            llServLimit = TRUE.

         LEAVE LOOP.
   END.

   RETURN llServLimit.
   
END FUNCTION.


