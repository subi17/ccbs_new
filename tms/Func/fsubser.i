/* fsubser.i        13.12.04/aam

   changes:         04.02.05/aam fSubSerValue, fCSSParamValue, fCallSpecDuring
                    24.08.05/aam ilSolog to fUpdateSubser(&Para)
*/
{Syst/commali.i}

DEF BUFFER bSubCur     FOR SubSer.
DEF BUFFER bSubParaCur FOR SubSerPara.
DEF BUFFER bHLRServCom FOR ServCom.

/* check status (value) of subscription service */
FUNCTION fSubSerValue RETURNS INTEGER
   (iiMsSeq   AS INT,
    icServCom AS CHAR,
    idtDate   AS DATE).

   DEF VAR liSSValue AS INT NO-UNDO.
   
   liSSValue = 0.
   
   FOR FIRST bSubCur NO-LOCK WHERE
             bSubCur.MsSeq    = iiMsSeq   AND
             bSubCur.ServCom  = icServCom AND
             bSubCur.SSDate  <= idtDate:
             
      liSSValue = bSubCur.SSStat.       
   END.

   RETURN liSSValue.

END FUNCTION.

/* check status (value) of currently valid subscription service */
FUNCTION fCurrSubSer RETURNS INTEGER
   (iiMsSeq   AS INT,
    icServCom AS CHAR).

   RETURN fSubSerValue(iiMsSeq,
                       icServCom,
                       TODAY).
END FUNCTION.

/* check parameter value of currently valid subscription service */
FUNCTION fSSParamValue RETURNS CHARACTER
   (iiMsSeq   AS INT,
    icServCom AS CHAR,
    idtDate   AS DATE).

   DEF VAR lcSSParam AS CHAR NO-UNDO.
   
   lcSSParam = "".
   
   FOR FIRST bSubCur NO-LOCK WHERE
             bSubCur.MsSeq    = iiMsSeq   AND
             bSubCur.ServCom  = icServCom AND
             bSubCur.SSDate  <= idtDate:
             
      IF bSubCur.SSStat > 0 THEN           
      lcSSParam = bSubCur.SSParam.       
   END.

   RETURN lcSSParam.

END FUNCTION.

/* check parameter value of currently valid subscription service */
FUNCTION fCurrSSParam RETURNS CHARACTER
   (iiMsSeq   AS INT,
    icServCom AS CHAR).

   RETURN fSSParamValue(iiMsSeq,
                        icServCom,
                        TODAY).
                        
END FUNCTION.

/* check parameter value of currently valid subscription service attribute */
FUNCTION fCurrSubSerPara RETURNS CHARACTER
   (iiMsSeq    AS INT,
    icServCom  AS CHAR,
    icServAttr AS CHAR).

   DEF VAR lcSSParam AS CHAR NO-UNDO.
   
   lcSSParam = "".
   
   FOR FIRST bSubParaCur NO-LOCK WHERE
             bSubParaCur.MsSeq    = iiMsSeq    AND
             bSubParaCur.ServCom  = icServCom  AND
             bSubParaCur.ParaName = icServAttr AND
             bSubParaCur.SSDate  <= TODAY:
             
      lcSSParam = bSubParaCur.ParaValue.       
   END.

   RETURN lcSSParam.

END FUNCTION.

/* update new value to subscription's service */
FUNCTION fUpdateSubSer RETURNS LOGICAL
   (iiMsSeq   AS INT,
    icServCom AS CHAR,
    idtDate   AS DATE,
    iiValue   AS INT,
    icParam   AS CHAR,
    ilSolog   AS LOG). 

   DO TRANS:     
      FIND FIRST bSubCur WHERE
                 bSubCur.MsSeq   = iiMsSeq AND
                 bSubCur.ServCom = icServCom NO-LOCK NO-ERROR.
                 
      /* no changes */
      IF AVAILABLE bSubCur AND
         bSubCur.SSStat  = iiValue AND
         bSubCur.SSParam = icParam 
      THEN RETURN FALSE. 
      
      /* if there already is a service with given date (or newer) 
         then override it */
      IF AVAILABLE bSubCur AND bSubCur.SSDate >= idtDate THEN
      FIND CURRENT bSubCur EXCLUSIVE-LOCK.
      
      ELSE DO:
         CREATE bSubCur.
         ASSIGN bSubCur.MsSeq   = iiMsSeq 
                bSubCur.ServCom = icServCom
                bSubCur.SSDate  = idtDate.
      END.

      /* if solog-update requested, check if it is needed */  
      IF ilSolog THEN DO:
         FIND bHLRServCom WHERE
              bHLRServCom.Brand   = gcBrand AND
              bHLRServCom.ServCom = bSubCur.ServCom NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bHLRServCom OR
            bHLRServCom.ActType > 0 
         THEN ilSolog = FALSE.
      END. 
 
      ASSIGN bSubCur.SSStat    = iiValue
             bSubCur.SSParam   = icParam
             bSubCur.SologStat = INTEGER(ilSolog).
             
      RELEASE bSubCur.        
   END.

   RETURN TRUE. 
              
END FUNCTION.

/* update new value to subscription's service attribute */
FUNCTION fUpdateSubSerPara RETURNS LOGICAL
   (iiMsSeq    AS INT,
    icServCom  AS CHAR,
    icServAttr AS CHAR,
    idtDate    AS DATE,
    icValue    AS CHAR,
    ilSolog   AS LOG). 

   DO TRANS:     
      FIND FIRST bSubCur WHERE
                 bSubCur.MsSeq   = iiMsSeq AND
                 bSubCur.ServCom = icServCom NO-LOCK NO-ERROR.
                 
      /* component not defined for this subscription */
      IF NOT AVAILABLE bSubCur THEN RETURN FALSE. 
   
      FIND FIRST bSubParaCur WHERE
                 bSubParaCur.MsSeq    = iiMsSeq   AND
                 bSubParaCur.ServCom  = icServCom AND
                 bSubParaCur.ParaName = icServAttr NO-LOCK NO-ERROR.
                 
      /* no changes */
      IF AVAILABLE bSubParaCur AND
         bSubParaCur.ParaValue = icValue 
      THEN RETURN FALSE. 
      
      /* if there already is a service attribute with given date (or newer) 
         then override it */
      IF AVAILABLE bSubParaCur AND bSubParaCur.SSDate >= idtDate THEN
      FIND CURRENT bSubParaCur EXCLUSIVE-LOCK.
      
      ELSE DO:
         CREATE bSubParaCur.
         ASSIGN bSubParaCur.MsSeq    = iiMsSeq 
                bSubParaCur.ServCom  = icServCom
                bSubParaCur.ParaName = icServAttr
                bSubParaCur.SSDate   = idtDate.
      END.

      /* if solog-update requested, check if it is needed */  
      IF ilSolog THEN DO:
         FIND bHLRServCom WHERE
              bHLRServCom.Brand   = gcBrand AND
              bHLRServCom.ServCom = bSubParaCur.ServCom NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bHLRServCom OR
            bHLRServCom.ActType > 0 
         THEN ilSolog = FALSE.
      END. 
 
      ASSIGN bSubParaCur.ParaValue = icValue
             bSubParaCur.SologStat = INTEGER(ilSolog). 
             
      RELEASE bSubParaCur.        
   END.

   RETURN TRUE. 
              
END FUNCTION.

/* secret */
FUNCTION fSecretValue RETURNS LOGICAL
   (iiMsSeq AS INT).
   
   RETURN (fCurrSubSer(iiMsSeq,"AES") > 0).
   
END FUNCTION.

/* number inquiry (actual value of parameter) */
FUNCTION fNumberInqValue RETURNS INTEGER
   (iiMsSeq AS INT).
   
   RETURN fCurrSubSer(iiMsSeq,"NUMBERINQ").

END.

/* number inquiry in verbal mode */
FUNCTION fNumberInqExpl RETURNS CHARACTER
   (iiMsSeq AS INT).
   
   CASE fNumberInqValue(iiMsSeq):
   WHEN 0 THEN RETURN "NOT SECRET".
   OTHERWISE   RETURN "SECRET".
   END CASE.
   
END.

/* credit type i.e. type of saldo service and limit for service */
FUNCTION fCreditTypeValue RETURNS INTEGER
   (INPUT  iiMsSeq AS INT,
    OUTPUT oiLimit AS INT).

   DEF VAR liCType AS INT NO-UNDO.

   oiLimit = fCurrSubSer(iiMsSeq,"SALDOAGR").
          
   IF oiLimit NE ? AND oiLimit > 0 
   THEN liCType = 3.
  
   ELSE DO:
      oiLimit = fCurrSubSer(iiMsSeq,"SALDOREM").

      IF oiLimit = ? THEN oiLimit = 0.
      IF oiLimit > 0 THEN liCType = 2.
   END. 
    
   RETURN liCType.
 
END FUNCTION.

/* notify number */
FUNCTION fNotifyNbrValue RETURNS CHARACTER
   (iiMsSeq AS INT).
   
   RETURN fCurrSSParam(iiMsSeq,"NTFNBR").

END.

/* current report code(s) for call specification */
FUNCTION fCallSpecReport RETURNS INTEGER
   (iiMsSeq AS INT):
   
   DEF VAR liSpec AS INT NO-UNDO.

   RETURN fSubSerValue(iiMsSeq,"CALLSPEC",TODAY).
END.

/* report code(s) for call specification on specific date */
FUNCTION fCallSpecDuring RETURNS INTEGER
   (iiMsSeq AS INT,
    idtDate AS DATE):
   
   DEF VAR liSpec AS INT NO-UNDO.

   RETURN fSubSerValue(iiMsSeq,"CALLSPEC",idtDate).
END.



 


