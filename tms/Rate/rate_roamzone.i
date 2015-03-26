FUNCTION fGetMcdrDtlValue RETURN CHARACTER
  (INPUT  idtDate  AS DATE ,
   INPUT  iidtlseq AS INT,
   INPUT  icTarget AS CHAR).

   DEF VAR liLoop   AS INT  NO-UNDO.
   DEF VAR lcTitle  AS CHAR NO-UNDO.
   DEF VAR lcValue  AS CHAR NO-UNDO.
   DEF VAR lcTemp   AS CHAR NO-UNDO.
   DEF VAR lcFormat AS CHAR No-UNDO.
   DEF VAR lcVersion AS CHAR NO-UNDO.

   FIND FIRST mcdrdtl.McdrDtl2 WHERE 
              mcdrdtl.McdrDtl2.Datest = idtDate AND 
              mcdrdtl.McdrDtl2.dtlseq = iidtlseq NO-LOCK NO-ERROR.

   IF NOT AVAILABLE mcdrdtl.McdrDtl2 THEN RETURN "".
   
   lcVersion = mcdrdtl.McdrDtl2.version.
   IF INDEX(lcVersion,"YC") = 0 THEN 
      lcVersion = lcVersion + ENTRY(4,mcdrdtl.McdrDtl2.Detail,"|").
 
   FIND FIRST CSVHeader WHERE
              CSVHeader.Version = lcVersion NO-LOCK NO-ERROR.

   IF AVAILABLE CSVHeader THEN 
   DO liLoop = 1 TO NUM-ENTRIES(CSVHeader.CSV,"|"):
      
      ASSIGN
      lcTemp   = ENTRY(liLoop,CSVHeader.CSV,"|")
      lcTitle  = REPLACE(ENTRY(2,lcTemp,"<"),">","")
      lcFormat = REPLACE(ENTRY(3,lcTemp,"<"),">","")
      lcTitle  = ENTRY(2,lcTitle,"=")
      lcFormat = ENTRY(2,lcFormat,"=")
      lcValue  = ENTRY(liLoop,mcdrdtl.McdrDtl2.detail,"|") NO-ERROR.
      IF lcTitle = icTarGet THEN LEAVE.

   END.

   Return lcvalue.

END.

FUNCTION fGetBRoamZone RETURN LOGICAL
  (INPUT  icNumber         AS CHARACTER,
   OUTPUT ocBRoamZone      AS CHARACTER):

   DEF VAR liLoop   AS INT  NO-UNDO.
   
   IF icNumber BEGINS "00"  THEN icNumber = SUBSTRING(icNumber,3).
   
   BNUMBER:
   DO liLoop = 5 TO 1 BY -1:

      FIND FIRST rzItem WHERE
                 rzItem.CountryPrefix = SUBSTRING(icNumber,1,liLoop)
      NO-LOCK NO-ERROR.

      IF Avail rzItem THEN DO:

         ASSIGN
            ocBRoamZone      = rzItem.RoamZone.

         RETURN TRUE.
      END.
   END.

   RETURN FALSE.

END.

FUNCTION fGetRoamZones RETURN LOGICAL
  (INPUT  icNumber         AS CHARACTER,
   INPUT  icPLMN           AS CHARACTER,
   OUTPUT ocARoamZone      AS CHARACTER,
   OUTPUT ocBRoamZone      AS CHARACTER):
   
   fGetBRoamZone(
      INPUT icNumber,
      OUTPUT ocBRoamZone).

   FIND FIRST rzItem WHERE
              rzItem.PlmnCode = icPLMN
   NO-LOCK NO-ERROR.

   IF AVAIL RZItem THEN ocARoamZone = rzItem.RoamZone.

END.

FUNCTION fGetRoamZones_MSRN RETURN LOGICAL
  (INPUT  icNumber         AS CHARACTER,
   INPUT  icMSRN           AS CHARACTER,
   OUTPUT ocARoamZone      AS CHARACTER,
   OUTPUT ocBRoamZone      AS CHARACTER).

   DEF VAR liLoop AS INT NO-UNDO. 

   fGetBRoamZone(
      INPUT icNumber,
      OUTPUT ocBRoamZone).

   icMSRN = LEFT-TRIM(icMSRN,"0").
   
   DO liLoop = MIN(4,LENGTH(icMSRN)) TO 1 BY -1:
      FOR EACH PLMN NO-LOCK USE-INDEX CountryPrefix WHERE
               PLMN.CountryPrefix = SUBSTRING(icMSRN,1,liLoop) AND
               PLMN.PLMN > "",
         FIRST RZItem NO-LOCK WHERE
               RZItem.PLMNCode = PLMN.PLMN:
         ocARoamZone = RZItem.RoamZone.
         LEAVE.
      END.
      IF ocARoamZone > "" THEN LEAVE.
   END.
   
END.


