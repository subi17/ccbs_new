/* ----------------------------------------------------------------------
  MODULE .......: barrfunc.i
  TASK .........: Barring functions
  APPLICATION ..: TMS
  AUTHOR .......: jukka 
  CREATED ......: 28.01.08
  CHANGED ......: 15.05.08 Case for manual acceptance  
  Version ......: xfera
-----------------------------------------------------------------------
10/01/2018 ashok YDR-2754 Added new function fGetBarringRequestSource to 
                          find request source of latest barring request
----------------------------------------------------------------------- */

&IF "{&barrfunc}" NE "YES"
&THEN

&GLOBAL-DEFINE barrfunc YES
{Syst/tmsconst.i}

DEFINE TEMP-TABLE ttBarringCmd NO-UNDO
   FIELD BarrCode AS CHAR
   FIELD BarrStatus AS CHAR
INDEX BarrCode IS PRIMARY UNIQUE BarrCode.

DEFINE TEMP-TABLE ttMergedBarring NO-UNDO
   FIELD BarrCode AS CHAR
   FIELD BarrStatus AS CHAR
   FIELD NWStatus AS CHAR
INDEX BarrCode IS PRIMARY UNIQUE BarrCode.

DEFINE TEMP-TABLE ttProvCommand NO-UNDO
   FIELD Component AS CHAR
   FIELD ComponentValue AS INT
   FIELD ComponentParam AS CHAR 
   FIELD BarringCmd AS CHAR
   FIELD FixedValue AS LOG
   FIELD DropService AS CHAR.

/*
fCheckBarrStatus:
Parameters:
   iiMsSeq           - Input, MsSeq
   ocActiveList      - Output, List of active barrings
   orBarring         - Output, REC ID for ongoing request
Return values:
   TRUE              - Error or network command is available
   FALSE             - No ongoing requests

*/
FUNCTION fCheckBarrStatus RETURNS LOGICAL
(INPUT iiMsSeq AS INTEGER,
 OUTPUT ocActiveList AS CHAR,
 OUTPUT orBarring AS ROWID):

   DEF BUFFER bBarrReq FOR MsRequest.
   DEF VAR llRet       AS LOGICAL NO-UNDO.

   orBarring = ?.
   llRet = FALSE.
   
 /* Check if any other than "completed" master request exists for this mobsub */
   FIND FIRST bBarrReq NO-LOCK WHERE
              bBarrReq.MsSeq = iiMsSeq      AND
              bBarrReq.ReqType = 35         AND
             (bBarrReq.ReqStat NE 2         AND
              bBarrReq.ReqStat NE 4         AND
              bBarrReq.ReqStat NE 9)
   NO-ERROR.

   /* Ongoing master request */
   IF AVAIL bBarrReq THEN DO:
      orBarring = ROWID(bBarrReq).
      llRet = TRUE. /* Error or ongoing network commands */
   END.

   /* Find active barrings */
   ocActiveList = Func.BarrMethod:mGetActiveBarrings(iiMsSeq).

   RETURN llRet.

END.

FUNCTION fCheckStatus RETURNS CHARACTER
(INPUT iiMsSeq AS INTEGER):

   DEF VAR lrBarring  AS ROWID NO-UNDO.
   DEF VAR lcList AS CHAR NO-UNDO.
   
   fCheckBarrStatus(iiMsseq, OUTPUT lcList, OUTPUT lrBarring).
   RETURN lcList.

END FUNCTION.

/*Function returns true if entry (or single item) in list1 is in list2.*/
/*Note: This filters =1 and =0 pars off to enable usage with commands*/
FUNCTION fIsInList RETURNS LOGICAL
( INPUT icList1 AS CHAR,
 INPUT icList2 AS CHAR):
  
   DEF VAR liCount AS INT NO-UNDO.
   icList1 = REPLACE(icList1, "=0",""). 
   icList1 = REPLACE(icList1, "=1","").
   icList2 = REPLACE(icList2, "=0",""). 
   icList2 = REPLACE(icList2, "=1","").
   
   DO liCount = 1 TO NUM-ENTRIES(icList1):
      IF LOOKUP(ENTRY(liCount, icList1), icList2) > 0
      THEN RETURN TRUE.
  END.
  RETURN FALSE.
END FUNCTION.

FUNCTION fGetActiveBarrList RETURNS CHAR
   (INPUT icBarrList AS CHAR):

   DEF VAR lcResultList AS CHAR NO-UNDO. 
   DEF VAR lcEntry AS CHAR NO-UNDO. 
   DEF VAR liLoop AS INT NO-UNDO. 

   DO liLoop = 1 TO NUM-ENTRIES(icBarrList):
      lcEntry = ENTRY(liLoop, icBarrList).
      IF INDEX(lcEntry,"=0") > 0 THEN NEXT.
      lcResultList = "," + lcResultList + lcEntry.
   END.

   RETURN SUBSTRING(lcResultList,2).

END.

FUNCTION fGetBarringUser RETURNS CHAR(
   INPUT iiMsSeq AS INTEGER,
   INPUT icBarring AS CHAR):

   DEF BUFFER Barring FOR Barring.

   FIND FIRST Barring NO-LOCK WHERE
              Barring.MsSeq = iiMsSeq AND
              Barring.BarringCode = icBarring USE-INDEX MsSeq NO-ERROR.
   IF AVAIL Barring AND
            Barring.BarringStatus NE {&BARR_STATUS_INACTIVE} THEN
      RETURN Barring.UserCode.
END.

FUNCTION fGetBarrAlowedPayment RETURNS CHAR(
   INPUT icBarring AS CHAR):
   
   DEF BUFFER BarringConf FOR BarringConf.

   FIND FIRST BarringConf NO-LOCK WHERE
              BarringConf.BarringStatus EQ {&BARR_RULE_STATUS_ACTIVE} AND
              BarringConf.BarringCode EQ icBarring NO-ERROR.
   IF AVAIL BarringConf THEN RETURN BarringConf.AllowedPaymentType.

   RETURN "".
END.

/*fGetBarringsByGroup:
INPUT icGroup  - group name
INPUT iiOption - What to list
                 0 contents of given group
                 1 contents of all but given group
                 2 all available barrings 
*/
FUNCTION fGetBarrByGroup RETURNS CHAR(
   INPUT icGroup AS CHAR,
   INPUT iiOption AS INT): 

   DEF VAR lcContents AS CHAR NO-UNDO.
   DEF BUFFER BarringConf FOR BarringConf.

   IF iiOption EQ 0 THEN DO:
      FOR EACH BarringConf NO-LOCK WHERE
               BarringConf.BarringGroup EQ icGroup AND
               BarringConf.BarringStatus EQ {&BARR_RULE_STATUS_ACTIVE}:
         lcContents = lcContents + BarringConf.BarringCode + ",".
      END.
   END.
   ELSE IF iiOption EQ 1 THEN DO:
      FOR EACH BarringConf NO-LOCK WHERE
               BarringConf.BarringGroup NE icGroup AND
               BarringConf.BarringStatus EQ {&BARR_RULE_STATUS_ACTIVE}:
         lcContents = lcContents + BarringConf.BarringCode + ",".
      END.
   END.
   ELSE DO:
      FOR EACH BarringConf NO-LOCK WHERE 
               BarringConf.BarringStatus EQ {&BARR_RULE_STATUS_ACTIVE}:
         lcContents = lcContents + BarringConf.BarringCode + ",".
      END.
   END.
   lcContents = RIGHT-TRIM(lcContents, ",").
   RETURN lcContents.
END.

FUNCTION fGetBarringsInGroup RETURNS CHAR
   (INPUT icGroup AS CHAR):
   RETURN fGetBarrByGroup(icGroup,0).
END.

FUNCTION fGetBarringsNotInGroup RETURNS CHAR
   (INPUT icGroup AS CHAR):
   RETURN fGetBarrByGroup(icGroup,1).
END.

FUNCTION fGetAllBarrings RETURNS CHAR
   (INPUT icGroup AS CHAR):
   RETURN fGetBarrByGroup(icGroup,2).
END.

/*Returns status of an individual barring.*/
FUNCTION fGetBarringStatus RETURNS CHAR
   (icBarring AS CHAR ,
    iiMsSeq AS INTEGER):

   DEF BUFFER Barring FOR Barring.

   FIND FIRST Barring NO-LOCK WHERE
              Barring.MsSeq EQ iiMsSeq AND
              Barring.BarringCode EQ icBarring NO-ERROR.
   IF AVAIL Barring THEN RETURN Barring.BarringStatus.
   ELSE RETURN {&BARR_STATUS_INACTIVE}.
END.

/* YDR-2754 return RequestSource of a Barring Request per MobSub */
FUNCTION fGetBarringRequestSource RETURNS CHARACTER 
    (INPUT iiMsseq         AS INTEGER   ,
     INPUT icBarringCode   AS CHARACTER ,
     INPUT icBarringStatus AS CHARACTER ):
    DEFINE BUFFER Barring   FOR Barring.
    DEFINE BUFFER Msrequest FOR MsRequest.
    
    FIND FIRST Barring NO-LOCK 
         WHERE Barring.MsSeq         = iiMsseq        AND
               Barring.BarringCode   = icBarringCode  AND
               Barring.BarringStatus BEGINS icBarringStatus NO-ERROR.
    IF NOT AVAILABLE Barring THEN RETURN "".
    FIND MsRequest NO-LOCK WHERE MsRequest.MsRequest = Barring.MsRequest NO-ERROR.
    IF NOT AVAILABLE MsRequest THEN RETURN "".
    RETURN MsRequest.ReqSource.
             
END FUNCTION.


/*Function checks that are requested barrings already on/off*/ 
FUNCTION fIsReasonableSet RETURNS LOG
   (icBarrlist AS CHAR,
    iiMsSeq AS INTEGER):

   DEF VAR liCount AS INT NO-UNDO.
   DEF VAR lcBarrCmd AS CHAR NO-UNDO. 

   IF icBarrlist EQ "#REFRESH" OR
      icBarrlist EQ "#REFRESH_BARRING" THEN RETURN TRUE.

   DO liCount = 1 TO NUM-ENTRIES(icBarrlist):

      lcBarrCmd = ENTRY(liCount, icBarrlist).

      IF R-INDEX(lcBarrCmd,"=1") > 0 THEN DO:
         lcBarrCmd = REPLACE(lcBarrCmd, "=1","").
         IF fGetBarringStatus(lcBarrCmd, iiMsSeq)
           EQ {&BARR_STATUS_ACTIVE} THEN RETURN FALSE.
      END.
      ELSE IF R-INDEX(lcBarrCmd,"=0") > 0 THEN DO:
         lcBarrCmd = REPLACE(lcBarrCmd, "=0","").
         IF fGetBarringStatus(lcBarrCmd, iiMsSeq)
            EQ {&BARR_STATUS_INACTIVE} THEN RETURN FALSE.
      END.
      ELSE RETURN FALSE. /*no =0 or =1 with command*/

   END.
   
   RETURN TRUE.
END.

/*
Function checks if a barring command list contains identical entries
This should stop adding process.
For example BARR=1,BARR=1 is not allowed.
*/
FUNCTION fIsAllowedOperation RETURNS LOG
   (icList AS CHAR):

   DEF VAR lcModList AS CHAR NO-UNDO.
   DEF VAR liC1 AS INT NO-UNDO.
   DEF VAR lic2 AS INT NO-UNDO.

   lcModList= REPLACE(icList, "=0","").
   lcModList= REPLACE(lcModList, "=1","").

   DO liC1 = 1 TO NUM-ENTRIES(lcModList) - 1:
      DO liC2 = lic1 + 1 TO NUM-ENTRIES(lcModList):
         IF ENTRY(liC1, lcModList) EQ ENTRY(liC2, lcModList) THEN
            RETURN FALSE. /*identical entries*/
      END.
   END.
   RETURN TRUE.
END.

/*Function checks that are requested barrings compatible with existing*/
FUNCTION fBarringExistWithCorrectStatus RETURNS LOG
   (icBarrlist AS CHAR):

   DEF VAR liCount AS INT NO-UNDO.
   DEF VAR lcBarrCmd AS CHAR NO-UNDO.
   DEF BUFFER BarringConf FOR BarringConf.

   DO liCount = 1 TO NUM-ENTRIES(icBarrlist):

      lcBarrCmd = ENTRY(liCount, icBarrlist).

      IF R-INDEX(lcBarrCmd,"=1") > 0 THEN DO:
         lcBarrCmd = REPLACE(lcBarrCmd, "=1","").
         FIND FIRST BarringConf NO-LOCK WHERE
                    BarringConf.BarringStatus EQ {&BARR_RULE_STATUS_ACTIVE} AND
                    BarringConf.BarringCode EQ lcBarrCmd NO-ERROR.
         IF NOT AVAIL BarringConf THEN RETURN FALSE.
      END.
      ELSE IF R-INDEX(lcBarrCmd,"=0") > 0 THEN DO:
         lcBarrCmd = REPLACE(lcBarrCmd, "=0","").
         FIND FIRST BarringConf NO-LOCK WHERE
                    BarringConf.BarringStatus 
                    NE {&BARR_RULE_STATUS_INACTIVE}  AND
                    BarringConf.BarringCode EQ lcBarrCmd NO-ERROR.
         IF NOT AVAIL BarringConf THEN RETURN FALSE.
      END.
      ELSE RETURN FALSE. /*no =0 or =1 with command*/
   END.
   
   RETURN TRUE.
END.

FUNCTION fGetFinalMask RETURNS CHAR
   (iiMsSeq AS INTEGER):

   DEF BUFFER mobsub FOR mobsub.

   FIND FIRST MobSub NO-LOCK WHERE
              MobSub.MsSeq EQ iiMsSeq NO-ERROR.
   IF AVAIL MobSub THEN RETURN MobSub.BarrCode.

   RETURN "0000000000000".
END.   

FUNCTION fValidateBarrRequest RETURNS CHAR
   ( INPUT iiMsSeq AS INT,
     INPUT icPackage AS CHAR):
   DEF VAR ocStatus AS CHAR NO-UNDO.
   DEF VAR llOngoing AS LOGICAL NO-UNDO.
   DEF VAR lcBarrList AS CHAR NO-UNDO.
   DEF VAR lrOngoing AS ROWID NO-UNDO.


   IF icPackage NE "#REFRESH" AND
      icPackage NE "#REFRESH_BARRING" THEN DO:
      /*Check that barring exists in configuration and status allows
        requested operation*/
      IF fBarringExistWithCorrectStatus( icPackage) EQ FALSE THEN DO:
         ocStatus = "Unknown barring or incompatible status".
         RETURN ocStatus.
      END.

      /*Check that is there confusion in barring list*/
      IF fIsAllowedOperation( icPackage) EQ FALSE THEN DO:
         ocStatus = "Incompatible command".
         RETURN ocStatus.
      END.

   END. /*Not #REFRESH*/
      
   /*Check that there is no ongoing request.*/
   llOngoing = fCheckBarrStatus(iiMsseq, 
                                OUTPUT lcBarrList, 
                                OUTPUT lrOngoing).
   IF llOngoing EQ TRUE THEN DO:
      ocStatus = "ONC".
      RETURN ocStatus.
   END.

   RETURN "".

END.     

/* Get IFS barring */
FUNCTION fIFSBarring RETURNS CHAR
   (INPUT icBarrList AS CHAR):

   DEF VAR liCount AS INT NO-UNDO.
   DEF VAR lcRetBarr AS CHAR NO-UNDO.
   DEF VAR liRetPriority AS INT NO-UNDO INIT 99999999.
   DEF VAR lcBarring AS CHAR NO-UNDO. 

   DEF BUFFER BarringConf FOR BarringConf.

   /*Return the most important entry in the list.*/
   DO liCount = 1 TO NUM-ENTRIES(icBarrList):

      lcBarring = ENTRY(liCount,icBarrList).
   
      IF INDEX(lcBarring,"=0") > 0 THEN NEXT.
      lcBarring = REPLACE(lcBarring, "=1","").

      FIND FIRST BarringConf NO-LOCK WHERE 
                 BarringConf.BarringCode EQ lcBarring
                 NO-ERROR.
      IF AVAIL BarringConf THEN DO:
         IF BarringConf.IFSPriority < liRetPriority THEN DO:
            liRetPriority = BarringConf.IFSPriority.
            lcRetBarr = (IF BarringConf.OldCode > ""
                         THEN BarringConf.OldCode 
                         ELSE BarringConf.BarringCode).
         END.   
      END.
   END.

   RETURN lcRetBarr.
END.



FUNCTION fExistBarredSubForCustomer RETURNS LOGICAL 
         (INPUT piCustNum AS INT): 
   
   DEF VAR lcActiveBarrings AS CHAR NO-UNDO.    
   DEF BUFFER MobSub FOR MobSub.

   FOR EACH MobSub NO-LOCK WHERE 
            MobSub.Brand = Syst.Var:gcBrand AND 
            MobSub.CustNum = piCustNum AND 
            MobSub.MsStatus = 8 : 
      IF Func.BarrMethod:mSubsHaveActiveBarring(MobSub.MsSeq,
                                                {&FRAUD_BARR_CODES})
      THEN RETURN TRUE.
   END.
   RETURN FALSE.
END FUNCTION.

/* Convert a comma separated barring command list to temp-table */
FUNCTION fBarringListToTT RETURNS LOG
   (icBarrings AS CHAR,
    OUTPUT TABLE ttBarringCmd,
    OUTPUT ocError AS CHAR):

   DEF BUFFER BarringConf FOR BarringConf.

   DEF VAR i AS INT NO-UNDO. 
   DEF VAR lcBarring AS CHAR NO-UNDO. 
   DEF VAR lcBarrCode AS CHAR NO-UNDO. 
   DEF VAR liBarrStatus AS INT NO-UNDO. 
   
   DO i = 1 TO NUM-ENTRIES(icBarrings):

      lcBarring = ENTRY(i,icBarrings).

      ASSIGN   
         lcBarrCode = TRIM(ENTRY(1,lcBarring,"="))
         liBarrStatus = INT(ENTRY(2,lcBarring,"=")) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN DO:
         ocError = "ERROR: parameter syntax".
         RETURN FALSE.
      END.

      FIND FIRST ttBarringCmd NO-LOCK WHERE
                 ttBarringCmd.BarrCode = lcBarrCode NO-ERROR.
      IF AVAIL ttBarringCmd THEN DO:
         ocError = "ERROR: duplicate parameter".
         RETURN FALSE.
      END.

      IF liBarrStatus NE 0 AND liBarrStatus NE 1 THEN DO:
         ocError = "ERROR: unsupported barring status".
         RETURN FALSE.
      END.

      FIND FIRST BarringConf NO-LOCK WHERE
                 BarringConf.BarringCode = lcBarrCode NO-ERROR.
      IF NOT AVAIL BarringConf THEN DO:
         ocError = SUBST("ERROR: unsupported barring code &1", lcBarrCode).
         RETURN FALSE.
      END.

      CREATE ttBarringCmd.
      ASSIGN
         ttBarringCmd.BarrCode = BarringConf.BarringCode
         ttBarringCmd.Barrstatus = (IF liBarrStatus EQ 1 THEN "ACTIVE" ELSE "INACTIVE").
   END.

   RETURN TRUE.
END.

FUNCTION fMergeMasks RETURNS CHAR
   (INPUT icBarrMask AS CHAR,
    INPUT icBarrMask2 AS CHAR):

   DEF VAR lcMergedMask AS CHAR NO-UNDO. 
   DEF VAR liDigit AS INT NO-UNDO. 

   DEF VAR i AS INT NO-UNDO. 
               
   DO i = 1 TO LENGTH(icBarrMask):

      IF SUBSTRING(icBarrMask,i,1) EQ "1" OR
         SUBSTRING(icBarrMask2,i,1) EQ "1" THEN liDigit = 1.
      ELSE IF SUBSTRING(icBarrMask,i,1) EQ "2" OR
              SUBSTRING(icBarrMask2,i,1) EQ "2" THEN liDigit = 2.
      ELSE liDigit = 0.
      
      lcMergedMask = lcMergedMask + STRING(liDigit).
   END.

   RETURN lcMergedMask.
END.

FUNCTION fGetComponentInfo RETURNS CHAR
   (INPUT icBarrCode AS CHAR,
   OUTPUT ocComponentValue AS CHAR,
   OUTPUT ocActParam AS CHAR):

   FIND FIRST BarringConf NO-LOCK WHERE
              BarringConf.BarringCode EQ icBarrcode NO-ERROR.
   IF AVAIL BarringConf THEN DO:
      ocComponentValue = BarringConf.NWComponent.
      ocActParam = BarringConf.NWActParam. 
      RETURN "".
   END.
   RETURN "Conf not found".
END.   

FUNCTION fBuildBarringCommand RETURNS LOG
   (INPUT icCLIType AS CHAR,
    INPUT icReqSource AS CHAR,
    INPUT TABLE ttMergedBarring,
    OUTPUT TABLE ttProvCommand,
    OUTPUT ocFinalMask AS CHAR,
    OUTPUT ocError AS CHAR):

   DEF BUFFER bHotLineBarring FOR ttMergedBarring.
   DEF BUFFER bttMergedBarring FOR ttMergedBarring.
   DEF BUFFER bBarringConf FOR BarringConf.
   DEF VAR lcComponent AS CHAR NO-UNDO.
   DEF VAR lcComponentParam AS CHAR NO-UNDO.
   DEF VAR lcHotlList AS CHAR NO-UNDO.
   DEF VAR lcHotl AS CHAR NO-UNDO.   
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR liHLC AS INT NO-UNDO.

   FIND FIRST bHotLineBarring WHERE
       LOOKUP(bHotLineBarring.BarrCode,"Debt_HOTLP,Debt_HOTL") > 0 AND
       LOOKUP(bHotLineBarring.NWStatus,"ACTIVE,EXISTING") > 0
   NO-ERROR.
               
   FOR EACH ttMergedBarring NO-LOCK WHERE
            ttMergedBarring.NWStatus NE "existing",
      FIRST BarringConf NO-LOCK WHERE
            BarringConf.BarringCode = ttMergedBarring.BarrCode:
      
      IF ocFinalMask EQ "" THEN
         ocFinalMask = FILL("0", LENGTH(BarringConf.Mask)).
   
      FIND FIRST ttProvCommand WHERE
                 ttProvCommand.Component = BarringConf.NWComponent NO-ERROR.

      CASE BarringConf.NWComponent:

         WHEN "BARRING" THEN DO:
            
            /* All voice - If some other barring (except Debt_*) is activated 
             * over Debt_HOTL then still ignore 11-Hotlining and apply other
             * bits (new and old barrings).*/
            
            /* All voice - If some other barring (except Debt_*) is activated 
             * over Debt_HOTLP then eliminate new barring mask in translation 
             * phase and only set of commands related to Debt_HOTLP barring 
             * will remain in network. */

            IF icCLIType NE "CONTM2" AND
               AVAIL bHotLineBarring THEN NEXT.
               
            IF NOT AVAIL ttProvCommand THEN DO:
               CREATE ttProvCommand.
               ASSIGN
                  ttProvCommand.Component       = "BARRING"
                  ttProvCommand.ComponentValue  = 1
                  ttProvCommand.ComponentParam  =
                     (IF ttMergedBarring.NWStatus EQ "INACTIVE"
                      THEN "0000000"
                      ELSE BarringConf.NWActParam).
            END.
            ELSE IF ttMergedBarring.NwStatus NE "INACTIVE" THEN
               ttProvCommand.ComponentParam = 
                  fMergeMasks(ttProvCommand.ComponentParam,
                              BarringConf.NWActParam).
      
            /* CONTM2 - if some other barring (except Debt_*) is activated over
             * Debt_HOTL then still ignore 11-Hotlining and apply other bits 
             * (new and old barrings) */
            
            /*  CONTM2 - If some other barring (except Debt_*) is activated 
             *  over Debt_HOTLP then recalculate the new mask for XFBARR
             *  command and apply to network. */
            IF icCLIType EQ "CONTM2" AND
               AVAIL bHotLineBarring THEN 
                ttProvCommand.ComponentParam =
                  fMergeMasks(ttProvCommand.ComponentParam,
                              (IF bHotLineBarring.BarrCode EQ "D_HOTLP"
                               THEN "1110010"
                               ELSE "0001001")).

            IF icCLIType EQ "CONTM2" THEN
               OVERLAY(ttProvCommand.ComponentParam,2) = "11".
      
            IF LOOKUP(ttMergedBarring.NWStatus,"ACTIVE,EXISTING") > 0 THEN 
               ocFinalMask = fMergeMasks(BarringConf.Mask, ocFinalMask).
         END.

         WHEN "LP" OR 
         WHEN "BPSUB" THEN DO:
            IF LOOKUP(ttMergedBarring.NWStatus, "ACTIVE") > 0
               THEN DO:
               
               IF NOT AVAIL ttProvCommand THEN DO:
                  CREATE ttProvCommand.
                  ASSIGN ttProvCommand.Component = BarringConf.NWComponent
                         ttProvCommand.ComponentValue = 1
                         ttProvCommand.ComponentParam = BarringConf.NWActParam.
               END.
               ELSE DO:

                  /* parallel act/deact*/
                  IF ttProvCommand.ComponentValue EQ 0 THEN . 
                  ELSE IF ttProvCommand.ComponentParam NE 
                          BarringConf.NWActParam THEN DO:
                     ocError = SUBST("ERROR:Conflicting barring components '&1' vs. '&2'",
                                    ttProvCommand.ComponentParam,
                                    BarringConf.NWActParam).
                     RETURN FALSE.

                  END.
               
                  ASSIGN
                     ttProvCommand.ComponentValue = 1
                     ttProvCommand.ComponentParam = BarringConf.NWActParam.
               END.
            END.
            ELSE IF NOT AVAIL ttProvCommand THEN DO:

               IF BarringConf.NWComponent NE "LP" OR
                 (BarringConf.NWComponent EQ "LP" AND
                  icReqSource EQ {&REQUEST_SOURCE_NEWTON}) THEN DO:

                  CREATE ttProvCommand.
                  ASSIGN
                     ttProvCommand.Component = BarringConf.NWComponent
                     ttProvCommand.ComponentValue = (IF BarringConf.NWDeactParam > "" THEN 1 ELSE 0)
                     ttProvCommand.ComponentParam = BarringConf.NWDeactParam.
               END.
            END.
            
            IF LOOKUP(ttMergedBarring.NWStatus,"ACTIVE") > 0 THEN 
               ocFinalMask = fMergeMasks(BarringConf.Mask, ocFinalMask).
         
         
         END.
         WHEN "HOTLINE" THEN DO:
            IF LOOKUP(ttMergedBarring.NWStatus,"ACTIVE") > 0 THEN DO:
               
               IF NOT AVAIL ttProvCommand THEN DO:
                  CREATE ttProvCommand.
                  ASSIGN ttProvCommand.Component = BarringConf.NWComponent
                         ttProvCommand.ComponentValue = 1
                         ttProvCommand.ComponentParam = BarringConf.NWActParam.
               END.
               ELSE DO:

                  /* parallel act/deact*/
                  IF ttProvCommand.ComponentValue EQ 0 THEN . 
                  ELSE IF ttProvCommand.ComponentParam NE 
                          BarringConf.NWActParam THEN DO:
                     ocError = SUBST("ERROR:Conflicting barring components '&1' vs. '&2'",
                                    ttProvCommand.ComponentParam,
                                    BarringConf.NWActParam).
                     RETURN FALSE.
                  END.
               
                  ASSIGN
                     ttProvCommand.ComponentValue = 1
                     ttProvCommand.ComponentParam = BarringConf.NWActParam.
               END.
                  
               /* CONTM2: If Debt_HOTL barring is activated, after calculating 
                * resulting barring, ignore 11-Hotlining bit and apply other
                * bits of Debt_HOTL barring. */
               IF BarringConf.BarringCode EQ "Debt_HOTL" AND
                  icCLIType EQ "CONTM2" THEN DO:

                  DELETE ttProvCommand.
      
                  FIND FIRST ttProvCommand WHERE
                             ttProvCommand.Component = "BARRING" NO-ERROR.

                  IF NOT AVAIL ttProvCommand THEN DO:
                     CREATE ttProvCommand.
                     ASSIGN
                        ttProvCommand.Component       = "BARRING"
                        ttProvCommand.ComponentValue  = 1
                        ttProvCommand.ComponentParam  = 
                           SUBSTRING(BarringConf.Mask,1,7).
                  END.
                  ELSE ttProvCommand.ComponentParam = 
                     fMergeMasks(ttProvCommand.ComponentParam,
                                 SUBSTRING(BarringConf.Mask,1,7)).
               
                  OVERLAY(ttProvCommand.ComponentParam,2) = "11".
               END.
               
               /* AV-If Debt_HOTL barring is activated over some other existing 
                * barring (except Debt_HOTLP) then remove previous mask from 
                * network before applying 11-Hotlining and ignore other bits of
                * Debt_HOTL barring mask. */
               
               /* AV-If Debt_HOTLP barring is activated over some other existing
                * barring then remove previous mask from network before applying
                * set of commands related to Debt_HOTLP barring. */
               IF icCLIType NE "CONTM2" AND
                  LOOKUP(BarringConf.BarringCode,"Debt_HOTL,Debt_HOTLP") > 0 
                  THEN DO:

                  FOR EACH bttMergedBarring NO-LOCK WHERE
                           LOOKUP(bttMergedBarring.NWStatus,
                                  "ACTIVE,EXISTING") > 0,
                     FIRST bBarringConf NO-LOCK WHERE
                           bBarringConf.BarringCode = 
                              bttMergedBarring.BarrCode AND
                           bBarringConf.NwComponent EQ "BARRING":
                     ttProvCommand.BarringCmd = "0000000". 
                     LEAVE.
                  END.
               END.
               
               IF icCLIType EQ "CONTM2" AND
                  BarringConf.BarringCode EQ "Debt_HOTLP" THEN
                  ttProvCommand.ComponentParam = "HOTL=1,HOTTYPE=HOTLP_SOLO".

            END.
            ELSE IF NOT AVAIL ttProvCommand THEN DO:

               IF BarringConf.BarringCode EQ "Debt_HOTL" AND
                  icCLIType EQ "CONTM2" THEN DO:
                  
                  FIND FIRST ttProvCommand WHERE
                             ttProvCommand.Component = "BARRING" NO-ERROR.

                  IF NOT AVAIL ttProvCommand THEN DO:
                     CREATE ttProvCommand.
                     ASSIGN
                        ttProvCommand.Component       = "BARRING"
                        ttProvCommand.ComponentValue  = 1
                        ttProvCommand.ComponentParam  = "0000000".
                  END.

                  OVERLAY(ttProvCommand.ComponentParam,2) = "11".

               END.
               ELSE DO:

                  CREATE ttProvCommand.
                  ASSIGN
                     ttProvCommand.Component = BarringConf.NWComponent
                     ttProvCommand.ComponentValue = 0
                     ttProvCommand.ComponentParam = BarringConf.NWDeactParam.
               
                  IF icCLIType EQ "CONTM2" AND
                     BarringConf.BarringCode EQ "Debt_HOTLP" THEN
                     ttProvCommand.ComponentParam = "HOTL=0,HOTTYPE=HOTLP_SOLO".
               END.
               
            END.
            
            IF LOOKUP(ttMergedBarring.NWStatus,"active") > 0 THEN 
               ocFinalMask = fMergeMasks(BarringConf.Mask, ocFinalMask).
         END.
         
         OTHERWISE DO:
            ocError = SUBST("ERROR:Unsupported barring component: &1",
                            BarringConf.NWComponent).
            RETURN FALSE.
         END.
      END.
      
   END.
   
   /*merge existing barring components */
   FOR EACH ttMergedBarring NO-LOCK WHERE
            ttMergedBarring.NWStatus EQ "EXISTING",
      FIRST BarringConf NO-LOCK WHERE
            BarringConf.BarringCode = ttMergedBarring.BarrCode:
         
      IF BarringConf.NWComponent EQ "BARRING" AND
         icCLIType NE "CONTM2" AND
         AVAIL bHotLineBarring THEN NEXT.

      FIND FIRST ttProvCommand WHERE 
                 ttProvCommand.Component = BarringConf.NWComponent NO-ERROR.

      IF AVAIL ttProvCommand THEN DO:

         IF ttProvCommand.FixedValue THEN NEXT.
               
         ASSIGN
            ttProvCommand.ComponentValue = 1
            ttProvCommand.ComponentParam = 
               fMergeMasks(ttProvCommand.ComponentParam,
                           BarringConf.NWActParam).
      END.

      ocFinalMask = fMergeMasks(BarringConf.Mask, ocFinalMask).
   END.

   /*Check if HRLP redirecction must be removed.*/
   /*Blocked: Debt_LP (Component LP) - Only one with LP component
              Debt_HOTLP (Read component data from config)*/
   lcHotlList = "Debt_LP,Debt_HOTLP".
   do liHLC = 1 to NUM-ENTRIES(lcHotlList):
      /*Special handling for CONTM2 needs*/
      lcHotl = ENTRY(liHLC,lcHotlList).
      IF icCLIType EQ "CONTM2" AND lcHotl EQ "Debt_HOTLP" THEN DO:
         FIND FIRST ttProvCommand WHERE
                    ttProvCommand.ComponentValue EQ 1 AND
                    ttProvCommand.ComponentParam EQ "HOTL=1,HOTTYPE=HOTLP_SOLO" AND
                    ttProvCommand.Component EQ "HOTLINE" NO-ERROR.
         IF AVAIL ttProvCommand THEN DO:
            ttProvCommand.DropService = "HRLP".
            RETURN TRUE.
         END. 
      END. 
      ELSE DO:
         lcErr = fGetComponentInfo(lcHotl, 
                                   OUTPUT lcComponent, 
                                   OUTPUT lcComponentParam).
         IF lcErr EQ "" THEN DO: 
            FIND FIRST ttProvCommand WHERE 
                       ttProvCommand.ComponentValue EQ 1 AND
                       ttProvCommand.ComponentParam EQ lcComponentParam AND
                       ttProvCommand.Component EQ lcComponent NO-ERROR.
               
            IF AVAIL ttProvCommand THEN DO:
               ttProvCommand.DropService = "HRLP".
               RETURN TRUE.
            END.
         END.      
      END.   
   END.   
   RETURN TRUE.
END.

/* Merge new barring commands with existing barrings */
FUNCTION fMergeBarrings RETURNS LOG
   (iiMsSeq AS INTEGER,
    INPUT TABLE ttBarringCmd,
    OUTPUT TABLE ttMergedBarring,
    OUTPUT ocResult AS CHAR):

   DEF BUFFER Barring FOR Barring.

   /* activatei new or inactivate existing one  */
   FOR EACH ttBarringCmd:
      
      CREATE ttMergedBarring.
      ASSIGN
         ttMergedBarring.BarrCode = ttBarringCmd.BarrCode
         ttMergedBarring.BarrStatus = ttBarringCmd.BarrStatus
         ttMergedBarring.NWStatus = ttBarringCmd.BarrStatus.
   END.
      
   /* existing */
   FOR EACH Barring NO-LOCK WHERE
            Barring.MsSeq EQ iiMsSeq
      USE-INDEX MsSeq BREAK BY Barring.BarringCode:

      IF NOT FIRST-OF(Barring.BarringCode) THEN NEXT.

      IF Barring.BarringStatus NE {&BARR_STATUS_ACTIVE} THEN NEXT.

      IF NOT CAN-FIND(FIRST ttMergedBarring WHERE
                            ttMergedBarring.BarrCode = Barring.BarringCode)
      THEN DO:

         CREATE ttMergedBarring.
         ASSIGN
            ttMergedBarring.BarrCode = Barring.BarringCode
            ttMergedBarring.BarrStatus = Barring.BarringStatus
            ttMergedBarring.NWStatus = "EXISTING".
      END.
   END.
      
   RETURN TRUE.

END.

/* collect existing barrings for the full or barring mask NW refresh */
FUNCTION fRefreshBarrings RETURNS LOG
   (iiMsSeq AS INTEGER,
    icCommand AS CHAR,
    OUTPUT TABLE ttMergedBarring):

   DEF BUFFER Barring FOR Barring.
   DEF BUFFER BarringConf FOR BarringConf.

   FOR EACH Barring NO-LOCK WHERE
            Barring.MsSeq = iiMsSeq 
      USE-INDEX MsSeq BREAK BY BarringCode:
      
      IF FIRST-OF(Barring.BarringCode) AND
         Barring.BarringStatus EQ {&BARR_STATUS_ACTIVE} THEN DO:
            
         FIND FIRST BarringConf NO-LOCK WHERE
                    BarringConf.BarringCode = Barring.BarringCode NO-ERROR.
         IF NOT AVAIL BarringConf THEN NEXT.

         IF icCommand EQ "#REFRESH_BARRING" AND
            BarringConf.NWComponent NE "BARRING" THEN NEXT.
         
         CREATE ttMergedBarring.
         ASSIGN
            ttMergedBarring.BarrCode = Barring.BarringCode
            ttMergedBarring.BarrStatus = Barring.BarringStatus
            ttMergedBarring.NWStatus = "ACTIVE".
      END.
   END.

   RETURN TRUE.
END.
&ENDIF
