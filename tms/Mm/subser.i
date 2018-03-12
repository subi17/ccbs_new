/* barrgrp.i            2004/jp
   subser validations   2008/as
*/

{Syst/tmsconst.i}
{Func/fctserval.i}

FUNCTION fSubSerSSStat RETURNS INT
(INPUT iiMsseq AS INTEGER,
 INPUT icServCom AS CHAR,
 INPUT iiNewSSStat AS INTEGER,
 OUTPUT ocError AS CHARACTER):
  
   DEF VAR iValueCount    AS INT NO-UNDO.
   DEF VAR cAllProfValues AS CHAR NO-UNDO.

   FIND FIRST ServCom where 
      ServCom.Brand   = Syst.Var:gcBrand    AND 
      ServCom.ServCom = icServCom NO-LOCK NO-ERROR.

   /* RES-885 National rouming traffic restrictions */
   IF ServCom.ServCom EQ "NW" THEN DO:
      /* Gather all profile values from TMSCodes to cAllProfValues
         comma separated list */
      iValueCount = 0.
      cAllProfValues = "".
      FOR EACH TMSCodes WHERE
               TMSCodes.TableName = "Customer" AND
               TMSCodes.FieldName = "NWProfiles" AND
               TMSCodes.CodeGroup = "NWProfile" AND
               TMSCodes.inUse = 1 NO-LOCK:
         IF cAllProfValues = "" THEN
            cAllProfValues = TMSCodes.CodeValue.
         ELSE
            cAllProfValues = cAllProfValues + "," + TMSCodes.CodeValue.
         iValueCount = iValueCount + 1.
      END.

      IF LOOKUP(STRING(iiNewSSStat),cAllProfValues,",") = 0 THEN DO:
         ocError = "The value must be within range "
                   + ENTRY(0,cAllProfValues)
                   +  " - "
                   + ENTRY(iValueCount,cAllProfValues)
                   + " !".
         RETURN 1.
      END.
   END.
   ELSE DO: 
      /* 1 */
      IF iiNewSSStat < ServCom.SCValueRange[1]  OR
         iiNewSSStat > ServCom.SCValueRange[2] THEN DO:

         ocError = "The value must be within range " 
                   + STRING(ServCom.ScValueRange[1]) 
                   +  " - " 
                   + STRING(ServCom.ScValueRange[2] )
                   + " !".

         RETURN 1.
      END.
   
      /* 2 */
      IF INDEX(icServcom,"DELAY") > 0 THEN  DO:

         IF LOOKUP(STRING(iiNewSSStat),"5,10,15,20,25,30") = 0
         THEN DO:
            ocError = "You can only choose from 5,10,15,20,25 and 30.".
            RETURN 2.   
         END.
      END.
   END.   

   RETURN 0.

END.

FUNCTION fSubSerValidate RETURNS INT
(INPUT iiMsseq AS INTEGER,
INPUT icServCom AS CHAR,
INPUT iiNewSSStat AS INTEGER,
OUTPUT ocError AS CHARACTER):

   DEF VAR liDefValue AS INT NO-UNDO.
   DEF VAR ok AS LOGICAL NO-UNDO.

   FIND FIRST MobSub WHERE MobSub.MsSeq = iiMsseq NO-LOCK NO-ERROR.

   FIND FIRST SubSer NO-LOCK WHERE
      SubSer.MsSeq = iiMsSeq AND
      SubSer.ServCom = icServCom NO-ERROR.
   
   /* 1 */
   IF LOOKUP(Subser.ServCom,"BCG,OBA,OBO,OBR,RSA",",") > 0 THEN DO:
      RETURN 1. 
   END.
   
   /* 2 */  
   /* can service be changed from here */
   liDefValue = fServComValue(MobSub.CLIType,
                              icServCom,
                              OUTPUT ok).
   IF liDefValue = ? OR
     (liDefValue = 0 AND NOT ok) THEN DO:
      RETURN 2.
   END.

   /* 3 */
   /* Check ongoing service requests */
   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq      = iiMsSeq AND
                     MsRequest.ReqType    = 1       AND
                     MsRequest.ReqCParam1 = SubSer.ServCom AND
                     LOOKUP(STRING(MsRequest.ReqStat),{&REQ_INACTIVE_STATUSES}) = 0) THEN DO:
      ocError = "There is an active change request for service." + CHR(10) + 
                "Change is not allowed before request is handled.".
      RETURN 3.
   END.

   RETURN 0. /* ok */

END FUNCTION.


/* 
   RES-885 NRTR (National rouming traffic restrictions). 
   Validate network profile in case of service_id "NW". 
*/
FUNCTION fSubSerValidateNW RETURNS INT
   (INPUT iiMsseq     AS INT,
    INPUT icServCom   AS CHAR,
    INPUT icNewSSStat AS CHAR,
    OUTPUT ocError    AS CHAR):

    DEF VAR liDefValue AS INT NO-UNDO.
    DEF VAR ok AS LOGICAL NO-UNDO.
    DEF VAR cAllProfValues   AS CHAR NO-UNDO.

    FIND FIRST MobSub WHERE MobSub.MsSeq = iiMsseq NO-LOCK NO-ERROR.

    FIND FIRST SubSer NO-LOCK WHERE
       SubSer.MsSeq = iiMsSeq AND
       SubSer.ServCom = icServCom NO-ERROR.

    /* 1 */
    /* Gather all profile values from TMSCodes to cAllProfValues
       comma separated list */
    cAllProfValues = "".
    FOR EACH TMSCodes WHERE 
             TMSCodes.TableName = "Customer" AND 
             TMSCodes.FieldName = "NWProfiles" AND
             TMSCodes.CodeGroup = "NWProfile" AND
             TMSCodes.inUse = 1 NO-LOCK:
      IF cAllProfValues = "" THEN
         cAllProfValues = TMSCodes.CodeValue.
      ELSE
         cAllProfValues = cAllProfValues + "," + TMSCodes.CodeValue.
    END.

    IF LOOKUP(icNewSSStat,cAllProfValues,",") = 0 THEN DO:
       ocError = "Illegal network profile.".
       RETURN 4.
    END.

    /* 2 */  
    /* can service be changed from here */
    liDefValue = fServComValue(MobSub.CLIType,
                               icServCom,
                               OUTPUT ok).
    IF liDefValue = ? OR
      (liDefValue = 0 AND NOT ok) THEN DO:
       RETURN 2.
    END.

    /* 3 */
    /* Check ongoing service requests */
    IF AVAIL SubSer THEN DO:
       IF CAN-FIND(FIRST MsRequest WHERE
                         MsRequest.MsSeq      = iiMsSeq AND
                         MsRequest.ReqType    = {&REQTYPE_SERVICE_CHANGE} AND
                         MsRequest.ReqCParam1 = SubSer.ServCom AND
                         LOOKUP(STRING(MsRequest.ReqStat),{&REQ_INACTIVE_STATUSES}) = 0) THEN DO:
          ocError = "There is an active change request for service." + CHR(10) +
                    "Change is not allowed before request is handled.".
          RETURN 3.
       END.
    END.
    ELSE DO:
       ocError = "Service not found.".
       RETURN 4.
    END.

    RETURN 0. /* ok */

END FUNCTION.

