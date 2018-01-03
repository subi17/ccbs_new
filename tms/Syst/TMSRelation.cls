 
 /*------------------------------------------------------------------------
    File        : TMSRelation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Jan 03 09:15:03 EET 2018
    Notes       : 
       
    Example use of the class:

    DEFINE VARIABLE objTMSRelation AS CLASS Syst.TMSRelation NO-UNDO.
    DEFINE VARIABLE objObject      AS CLASS Progress.Lang.Object NO-UNDO.

    objTMSRelation = NEW Syst.objTMSRelation("DiscountPlan", "Compability").

    objTMSRelation:mSetSerializeNames("old_value",
                                      "new_value",
                                      "role",
                                      "validfrom",
                                      "validto",
                                      NO).

    /* To populate everything for "DiscountPlan" and "Compability" */  
    objTMSRelation:mPopulateData().

    /* To populate data where ParentValue = PARENTDISC*/  
    objTMSRelation:mPopulateData("PARENTDISC").

    /* To populate data where ParentValue = PARENTDISC and ChildValue = CHILDDISC */  
    objTMSRelation:mPopulateData("PARENTDISC", "CHILDDISC").

    objObject = objTMSRelation:mSerializeToJsonObject().
    
    FINALLY:
       IF VALID-OBJECT(objObject)
       THEN DELETE OBJECT objObject.
       
       IF VALID-OBJECT(objTMSRelation)
       THEN DELETE OBJECT objTMSRelation.
    END FINALLY.
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Syst.TMSRelation:

   DEFINE PRIVATE VARIABLE acTableName      AS CHARACTER   NO-UNDO.   
   DEFINE PRIVATE VARIABLE acKeyType        AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE acParentValue    AS CHARACTER   NO-UNDO.   
   DEFINE PRIVATE VARIABLE acChildValue     AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE alUseCurrentTime AS LOGICAL INITIAL FALSE NO-UNDO.

   DEFINE PRIVATE VARIABLE atzFromTime      AS DATETIME-TZ NO-UNDO.   
   DEFINE PRIVATE VARIABLE atzToTime        AS DATETIME-TZ NO-UNDO.   

   DEFINE PRIVATE TEMP-TABLE ttTMSRelation NO-UNDO SERIALIZE-NAME ""
      FIELD ParentValue  LIKE TMSRelation.ParentValue
      FIELD ChildValue   LIKE TMSRelation.ChildValue
      FIELD RelationType LIKE TMSRelation.RelationType
      FIELD ToTime       LIKE TMSRelation.ToTime
      FIELD FromTime     LIKE TMSRelation.FromTime
      FIELD ToDate       AS DATE
      FIELD FromDate     AS DATE
      INDEX ParentValue IS PRIMARY UNIQUE ParentValue ChildValue ToTime DESCENDING
      INDEX ChildValue ChildValue ParentValue ToTime DESCENDING.

   CONSTRUCTOR PUBLIC TMSRelation
      (icTableName           AS CHARACTER,
       icKeyType             AS CHARACTER):

      THIS-OBJECT(icTableName, icKeyType, NOW, NOW).   
      alUseCurrentTime = TRUE.

   END CONSTRUCTOR.

   CONSTRUCTOR PUBLIC TMSRelation
      (icTableName AS CHARACTER,
       icKeyType   AS CHARACTER,
       itzFromTime AS DATETIME-TZ,
       itzToTime   AS DATETIME-TZ):

      ASSIGN
         acTableName           = icTableName
         acKeyType             = icKeyType
         atzFromTime           = itzFromTime
         atzToTime             = itzToTime
         .

   END CONSTRUCTOR.

   DESTRUCTOR TMSRelation():
      EMPTY TEMP-TABLE ttTMSRelation.
   END DESTRUCTOR.

   METHOD PUBLIC VOID mSetSerializeNames
      (icParentValueName     AS CHARACTER,
       icChildValueName     AS CHARACTER,
       icRelationTypeName  AS CHARACTER,
       icFromTimeName      AS CHARACTER,
       icToTimeName        AS CHARACTER,
       ilUseDateTimeTz     AS LOGICAL):

      DEFINE VARIABLE lhBufferHandle AS HANDLE NO-UNDO.
      
      lhBufferHandle = BUFFER ttTMSRelation:HANDLE.

      IF ilUseDateTimeTz
      THEN ASSIGN
              lhBufferHandle:BUFFER-FIELD("FromDate"):SERIALIZE-HIDDEN = TRUE    
              lhBufferHandle:BUFFER-FIELD("ToDate"):SERIALIZE-HIDDEN = TRUE
              lhBufferHandle:BUFFER-FIELD("FromTime"):SERIALIZE-NAME = icFromTimeName
              lhBufferHandle:BUFFER-FIELD("ToTime"):SERIALIZE-NAME = icToTimeName.
      ELSE ASSIGN
              lhBufferHandle:BUFFER-FIELD("FromTime"):SERIALIZE-HIDDEN = TRUE    
              lhBufferHandle:BUFFER-FIELD("ToTime"):SERIALIZE-HIDDEN   = TRUE    
              lhBufferHandle:BUFFER-FIELD("FromDate"):SERIALIZE-NAME = icFromTimeName
              lhBufferHandle:BUFFER-FIELD("ToDate"):SERIALIZE-NAME = icToTimeName.

      ASSIGN
         lhBufferHandle:BUFFER-FIELD("ParentValue"):SERIALIZE-NAME = icParentValueName
         lhBufferHandle:BUFFER-FIELD("ChildValue"):SERIALIZE-NAME = icParentValueName
         lhBufferHandle:BUFFER-FIELD("RelationType"):SERIALIZE-NAME = icRelationTypeName.

   END METHOD.

   METHOD PUBLIC VOID mPopulateData():

      mPopulateData(?, ?).

   END METHOD.
   
   METHOD PUBLIC VOID mPopulateData
      (icParentValue AS CHARACTER):
         
      mPopulateData(icParentValue, ?).

   END METHOD.


   METHOD PUBLIC VOID mPopulateData
      (icParentValue AS CHARACTER,
       icChildValue  AS CHARACTER):
         
      mPopulateData(icParentValue, icChildValue).

   END METHOD.

   METHOD PUBLIC VOID mPopulateData
      (icParentValue         AS CHARACTER,
       icChildValue          AS CHARACTER):

      EMPTY TEMP-TABLE ttTMSRelation.

      IF alUseCurrentTime
      THEN ASSIGN
             atzFromTime = NOW
             atzToTime   = NOW.
             
      ASSIGN
          acParentValue = icParentValue
          acChildValue = icChildValue.

      IF icParentValue = ? AND icChildValue = ?     
      THEN FOR EACH TMSRelation NO-LOCK USE-INDEX ToTime WHERE
               TMSRelation.TableName = acTableName AND
               TMSRelation.KeyType   = acKeyType:
         
               IF NOT mCreateTT()
               THEN LEAVE.
           END.
      ELSE FOR EACH TMSRelation NO-LOCK USE-INDEX ParentValue WHERE
               TMSRelation.TableName = acTableName AND
               TMSRelation.KeyType   = acKeyType   AND
               TMSRelation.ParentValue = icParentValue:
               
               IF acChildValue NE ? AND TMSRelation.ChildValue NE acChildValue
               THEN NEXT.
               
               IF NOT mCreateTT()
               THEN LEAVE.
           END.

   END METHOD.

   METHOD PRIVATE LOGICAL mCreateTT():

      IF TMSRelation.ToTime NE ? AND
         atzToTime        NE ? AND
         TMSRelation.ToTime <  atzToTime
      THEN RETURN FALSE.
      
      IF atzToTime        EQ ? AND
         TMSRelation.ToTime NE ?
      THEN RETURN FALSE.
      
      IF atzFromTime        EQ ? AND 
         TMSRelation.FromTime NE ?
      THEN RETURN FALSE.

      IF TMSRelation.FromTime NE ? AND
         atzFromTime        NE ? AND
         TMSRelation.FromTime > atzFromTime
      THEN RETURN FALSE.
               
      CREATE ttTMSRelation.
      ASSIGN
         ttTMSRelation.ParentValue  = TMSRelation.ParentValue
         ttTMSRelation.ChildValue   = TMSRelation.ChildValue
         ttTMSRelation.RelationType = IF TMSRelation.RelationType = "ParentValue"
                                      THEN TMSRelation.ParentValue
                                      ELSE IF TMSRelation.RelationType = "ChildValue"
                                      THEN TMSRelation.ChildValue
                                      ELSE TMSRelation.RelationType
         ttTMSRelation.FromDate     = DATE(TMSRelation.FromTime)
         ttTMSRelation.ToDate       = DATE(TMSRelation.ToTime)
         ttTMSRelation.FromTime     = TMSRelation.FromTime
         ttTMSRelation.ToTime       = TMSRelation.ToTime
         .

      RETURN TRUE.
   
   END METHOD.

   METHOD PUBLIC JsonObject mSerializeToJsonObject():

      DEFINE VARIABLE objJsonObject AS CLASS JsonObject NO-UNDO.
      objJsonObject = NEW JsonObject().
      objJsonObject:Read(TABLE-HANDLE ttTMSRelation).

      RETURN objJsonObject.

   END METHOD.

END CLASS.