 
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

    objTMSRelation = NEW Syst.TMSRelation("DiscountPlan", "Compatibility").

    objTMSRelation:mSetSerializeNames("id",
                                      "old_value",
                                      "new_value",
                                      "role",
                                      "validfrom",
                                      "validto",
                                      NO).


     /* Set time range where recods can must be active. By default it is
        the current moment */
     objTMSRelation:mSetTimeRange(TODAY - 7, TODAY).

    /* To populate everything for "DiscountPlan" and "Compatibility" within the
       given time range */
    objTMSRelation:mPopulateData().

    /* To populate data where ParentValue = PARENTDISC*/  
    objTMSRelation:mPopulateData("PARENTDISC").

    /* To populate data where ParentValue = PARENTDISC and ChildValue = CHILDDISC */  
    objTMSRelation:mPopulateData("PARENTDISC", "CHILDDISC").

    /* Serialize whole data */
    objObject = objTMSRelation:mSerialize().
    
    /* Serialize only one id */
    objObject = objTMSRelation:mSerialize(/* TMSRelationID number */).

    FINALLY:
       IF VALID-OBJECT(objObject)
       THEN DELETE OBJECT objObject.
       
       IF VALID-OBJECT(objTMSRelation)
       THEN DELETE OBJECT objTMSRelation.
    END FINALLY.
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Syst.TMSRelation:

   DEFINE PRIVATE VARIABLE alThereWereOrdering AS LOGICAL     NO-UNDO.
   DEFINE PRIVATE VARIABLE alIncludeBrand      AS LOGICAL     NO-UNDO.
   DEFINE PRIVATE VARIABLE acTableName         AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE acKeyType           AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE acParentValue       AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE acChildValue        AS CHARACTER   NO-UNDO.

   DEFINE PRIVATE VARIABLE atzFromTime      AS DATETIME-TZ NO-UNDO.   
   DEFINE PRIVATE VARIABLE atzToTime        AS DATETIME-TZ NO-UNDO.   

   DEFINE PRIVATE TEMP-TABLE ttTMSRelation NO-UNDO SERIALIZE-NAME ""
      FIELD IDBrand       AS   CHARACTER INITIAL '&1|&2'
      FIELD TMSRelationID LIKE TMSRelation.TMSRelationID
      FIELD ParentValue   LIKE TMSRelation.ParentValue
      FIELD ChildValue    LIKE TMSRelation.ChildValue
      FIELD RelationType  LIKE TMSRelation.RelationType
      FIELD ToTime        LIKE TMSRelation.ToTime
      FIELD FromTime      LIKE TMSRelation.FromTime
      FIELD ToDate        AS DATE
      FIELD FromDate      AS DATE
      FIELD OrderNbr      AS INTEGER INITIAL ? SERIALIZE-HIDDEN
      INDEX OrderNbr IS PRIMARY OrderNbr TMSRelationID
      INDEX ParentValue IS UNIQUE ParentValue ChildValue ToTime DESCENDING
      INDEX TMSRelationID IS UNIQUE TMSRelationID
      INDEX ChildValue ChildValue ParentValue ToTime DESCENDING.

   DEFINE PUBLIC PROPERTY RecordCount AS INTEGER NO-UNDO
      GET.
      PRIVATE SET.

   CONSTRUCTOR PUBLIC TMSRelation
      (icTableName           AS CHARACTER,
       icKeyType             AS CHARACTER):

      ASSIGN
         acTableName = icTableName
         acKeyType   = icKeyType.

      mSetTimeRange(NOW, NOW).

   END CONSTRUCTOR.

   DESTRUCTOR TMSRelation():

      mEraseData().

   END DESTRUCTOR.

   METHOD PUBLIC VOID mEraseData():

      EMPTY TEMP-TABLE ttTMSRelation.

   END METHOD.

   METHOD PUBLIC VOID mDeleteNotMarked():

      FOR EACH ttTMSRelation WHERE ttTMSRelation.OrderNbr EQ ?:
         DELETE ttTMSRelation.
      END.

   END METHOD.

   METHOD PUBLIC VOID mSetTimeRange
      (itzFromTime AS DATETIME-TZ,
       itzToTime   AS DATETIME-TZ):

      IF itzFromTime > itzToTime
      THEN RETURN ERROR NEW Progress.Lang.AppError("FromTime cannot be greater than ToTime", 1).

      ASSIGN
         atzFromTime = itzFromTime
         atzToTime   = itzToTime
         .

   END METHOD.

   METHOD PUBLIC VOID mSetTimeRange
      (idaFromDate AS DATE,
       idaToDate   AS DATE):

      IF idaFromDate > idaToDate
      THEN RETURN ERROR NEW Progress.Lang.AppError("FromDate cannot be greater than ToDate", 1).

      ASSIGN
         atzFromTime = DATETIME-TZ(idaFromDate)
         atzToTime   = DATETIME-TZ(idaToDate)
         .

   END METHOD.

   METHOD PUBLIC VOID mSetSerializeNames
      (icTMSRelationIDName AS CHARACTER,
       icParentValueName   AS CHARACTER,
       icChildValueName    AS CHARACTER,
       icRelationTypeName  AS CHARACTER,
       icFromTimeName      AS CHARACTER,
       icToTimeName        AS CHARACTER,
       ilUseDateTimeTz     AS LOGICAL,
       ilIncludeBrand      AS LOGICAL):

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

      IF ilIncludeBrand
      THEN ASSIGN
              lhBufferHandle:BUFFER-FIELD("TMSRelationID"):SERIALIZE-HIDDEN = TRUE
              lhBufferHandle:BUFFER-FIELD("IDBrand"):SERIALIZE-NAME = icTMSRelationIDName.
      ELSE ASSIGN
              lhBufferHandle:BUFFER-FIELD("IDBrand"):SERIALIZE-HIDDEN = TRUE
              lhBufferHandle:BUFFER-FIELD("TMSRelationID"):SERIALIZE-NAME = icTMSRelationIDName.

      ASSIGN
         alIncludeBrand = ilIncludeBrand
         lhBufferHandle:BUFFER-FIELD("TMSRelationID"):SERIALIZE-NAME = icTMSRelationIDName
         lhBufferHandle:BUFFER-FIELD("ParentValue"):SERIALIZE-NAME = icParentValueName
         lhBufferHandle:BUFFER-FIELD("ChildValue"):SERIALIZE-NAME = icChildValueName
         lhBufferHandle:BUFFER-FIELD("RelationType"):SERIALIZE-NAME = icRelationTypeName.

   END METHOD.

   METHOD PUBLIC VOID mPopulateBrand
      (icBrand AS CHARACTER):

      mPopulateData(?, ?, icBrand).

   END METHOD.

   METHOD PUBLIC VOID mPopulateBrand
      (icBrand       AS CHARACTER,
       icParentValue AS CHARACTER):

      mPopulateData(icParentValue, ?, icBrand).

   END METHOD.

   METHOD PUBLIC VOID mPopulateBrand
      (icBrand       AS CHARACTER,
       icParentValue AS CHARACTER,
       icChildValue  AS CHARACTER):

      mPopulateData(icParentValue, icChildValue, icBrand).

   END METHOD.

   METHOD PUBLIC VOID mPopulateData():

      mPopulateData(?, ?, ?).

   END METHOD.

   METHOD PUBLIC VOID mPopulateData
      (icParentValue AS CHARACTER):

      mPopulateData(icParentValue, ?, ?).

   END METHOD.

   METHOD PUBLIC VOID mPopulateData
      (icParentValue         AS CHARACTER,
       icChildValue          AS CHARACTER):

      mPopulateData(icParentValue, icChildValue, ?).

   END METHOD.

   METHOD PUBLIC VOID mPopulateData
      (icParentValue         AS CHARACTER,
       icChildValue          AS CHARACTER,
       icBrand               AS CHARACTER):

      IF icBrand EQ ?
      THEN EMPTY TEMP-TABLE ttTMSRelation.

      ASSIGN
          RecordCount   = 0
          acParentValue = icParentValue
          acChildValue  = icChildValue.

      IF icParentValue = ? AND icChildValue = ?     
      THEN FOR EACH TMSRelation NO-LOCK USE-INDEX ToTime WHERE
               TMSRelation.TableName = acTableName AND
               TMSRelation.KeyType   = acKeyType:
         
               IF NOT mCreateTT(icBrand)
               THEN LEAVE.
           END.
      ELSE FOR EACH TMSRelation NO-LOCK USE-INDEX ParentValue WHERE
               TMSRelation.TableName = acTableName AND
               TMSRelation.KeyType   = acKeyType   AND
               TMSRelation.ParentValue = icParentValue:
               
               IF acChildValue NE ? AND TMSRelation.ChildValue NE acChildValue
               THEN NEXT.
               
               IF NOT mCreateTT(icBrand)
               THEN LEAVE.
           END.

   END METHOD.

   METHOD PRIVATE LOGICAL mCreateTT
      (icBrand AS CHARACTER):

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
      THEN RETURN TRUE.
               
      CREATE ttTMSRelation.

      BUFFER-COPY TMSRelation
         USING ParentValue ChildValue FromTime ToTime TMSRelationID
         TO ttTMSRelation
         ASSIGN
            ttTMSRelation.IDBrand      = SUBSTITUTE(ttTMSRelation.IDBrand, TMSRelation.TMSRelationID, icBrand) WHEN icBrand > ""
            ttTMSRelation.RelationType = IF TMSRelation.RelationType = "ParentValue"
                                         THEN TMSRelation.ParentValue
                                         ELSE IF TMSRelation.RelationType = "ChildValue"
                                         THEN TMSRelation.ChildValue
                                         ELSE TMSRelation.RelationType
            ttTMSRelation.FromDate     = DATE(TMSRelation.FromTime)
            ttTMSRelation.ToDate       = DATE(TMSRelation.ToTime)
            RecordCount                = RecordCount + 1
            .

      RETURN TRUE.
   
   END METHOD.

   METHOD PUBLIC VOID mSerialize
      ( icLevel     AS CHARACTER,
        objTTXMLRPC AS CLASS Syst.TempTableXMLRPC ):

      IF alThereWereOrdering
      THEN mDeleteNotMarked().

      objTTXMLRPC:mSerialize(icLevel, BUFFER ttTMSRelation:HANDLE).

   END METHOD.

   METHOD PUBLIC CLASS JsonArray mSerialize():

      IF alThereWereOrdering
      THEN mDeleteNotMarked().

      DEFINE VARIABLE objJsonArray AS CLASS JsonArray NO-UNDO.

      objJsonArray = NEW JsonArray().
      objJsonArray:Read(TEMP-TABLE ttTMSRelation:HANDLE).

      RETURN objJsonArray.

   END METHOD.

   METHOD PUBLIC CLASS JsonObject mSerialize
      (iiTMSRelationID AS INT64):

      FIND TMSRelation NO-LOCK WHERE TMSRelation.TMSRelationID = iiTMSRelationID NO-ERROR.
      IF NOT AVAILABLE TMSRelation
      THEN RETURN ERROR NEW Progress.Lang.AppError
               (SUBSTITUTE("Cannot find TMSRelationID &1",TMSRelation.TMSRelationID), 1).

      DEFINE VARIABLE objJsonObject AS CLASS JsonObject NO-UNDO.

      objJsonObject = NEW JsonObject().
      objJsonObject:Read(BUFFER TMSRelation:HANDLE).

      RETURN objJsonObject.

   END METHOD.

   METHOD PUBLIC VOID mMark
      ( iiTMSRelationID AS INT64,
        iiOrderNbr      AS INTEGER ):

      FIND ttTMSRelation WHERE ttTMSRelation.TMSRelationID = iiTMSRelationID NO-ERROR.

      IF NOT AVAILABLE ttTMSRelation
      THEN RETURN ERROR NEW Progress.Lang.AppError
               (SUBSTITUTE("Cannot mark TMSRelationID &1",TMSRelation.TMSRelationID), 1).

      ASSIGN
         ttTMSRelation.OrderNbr = iiOrderNbr
         alThereWereOrdering    = TRUE.

   END METHOD.

   METHOD PUBLIC STATIC VOID mAddKeyType
      (icTableName AS CHARACTER,
       icKeyType   AS CHARACTER):

      DEFINE VARIABLE ltzFromTime AS DATETIME-TZ NO-UNDO.

      DEFINE BUFFER TMSRelation FOR TMSRelation.

      FOR EACH TMSRelation NO-LOCK WHERE
               TMSRelation.TableName   = "TMSRelation" AND
               TMSRelation.KeyType     = "KeyType"     AND
               TMSRelation.ChildValue  = icKeyType     AND
               TMSRelation.ParentValue = icTableName
               BY TMSRelation.ToTime DESCENDING:
         IF TMSRelation.ToTime = ? OR TMSRelation.ToTime >= NOW
         THEN RETURN.

         ltzFromTime = ADD-INTERVAL(TMSRelation.ToTime, 1, 'milliseconds').
         LEAVE.
      END.

      CREATE TMSRelation.
      ASSIGN
         TMSRelation.TableName    = "TMSRelation"
         TMSRelation.KeyType      = "KeyType"
         TMSRelation.ChildValue   = icKeyType
         TMSRelation.ParentValue  = icTableName
         TMSRelation.FromTime     = ltzFromTime
         .

   END METHOD.

   /* Adds relation if it doesn't exists already */
   METHOD PUBLIC STATIC VOID mAddRelation
      (icTableName    AS CHARACTER,
       icKeyType      AS CHARACTER,
       icParentValue  AS CHARACTER,
       icChildValue   AS CHARACTER,
       ilUseDate      AS LOGICAL):

      mAddUpdateRelation(YES, icTableName, icKeyType, icParentValue, icChildValue, "", ilUseDate).

   END METHOD.

   /* Adds relation if it doesn't exists already */
   METHOD PUBLIC STATIC VOID mAddRelation
      (icTableName    AS CHARACTER,
       icKeyType      AS CHARACTER,
       icParentValue  AS CHARACTER,
       icChildValue   AS CHARACTER,
       icRelationType AS CHARACTER,
       ilUseDate      AS LOGICAL):

      mAddUpdateRelation(YES, icTableName, icKeyType, icParentValue, icChildValue, icRelationType, ilUseDate).

   END METHOD.

   /* Finish the old relation and starts the new relation. If the relation
      doesn't exists then just adds the relation. */
   METHOD PUBLIC STATIC VOID mUpdateRelation
      (icTableName    AS CHARACTER,
       icKeyType      AS CHARACTER,
       icParentValue  AS CHARACTER,
       icChildValue   AS CHARACTER,
       ilUseDate      AS LOGICAL):

      mAddUpdateRelation(NO, icTableName, icKeyType, icParentValue, icChildValue, "", ilUseDate).

   END METHOD.

   /* Finish the old relation and starts the new relation. If the relation
      doesn't exists then just adds the relation. */
   METHOD PUBLIC STATIC VOID mUpdateRelation
      (icTableName    AS CHARACTER,
       icKeyType      AS CHARACTER,
       icParentValue  AS CHARACTER,
       icChildValue   AS CHARACTER,
       icRelationType AS CHARACTER,
       ilUseDate      AS LOGICAL):

      mAddUpdateRelation(NO, icTableName, icKeyType, icParentValue, icChildValue, icRelationType, ilUseDate).

   END METHOD.

   METHOD PRIVATE STATIC VOID mAddUpdateRelation
      (ilAdd          AS LOGICAL,
       icTableName    AS CHARACTER,
       icKeyType      AS CHARACTER,
       icParentValue  AS CHARACTER,
       icChildValue   AS CHARACTER,
       icRelationType AS CHARACTER,
       ilUseDate      AS LOGICAL):

      DEFINE VARIABLE ltzFromTime    AS DATETIME-TZ NO-UNDO.
      DEFINE VARIABLE liRelationID   AS INT64       NO-UNDO.

      DEFINE BUFFER TMSRelation FOR TMSRelation.

      IF ilUseDate
      THEN ltzFromTime = DATETIME-TZ(TODAY, 0).
      ELSE ltzFromTime = NOW.

      FOR EACH TMSRelation NO-LOCK WHERE
               TMSRelation.TableName   = icTableName   AND
               TMSRelation.KeyType     = icKeyType     AND
               TMSRelation.ChildValue  = icChildValue  AND
               TMSRelation.ParentValue = icParentValue
               BY TMSRelation.ToTime DESCENDING:
         IF TMSRelation.ToTime = ? OR TMSRelation.ToTime >= ltzFromTime
         THEN DO:
            IF ilAdd
            THEN RETURN.
            ELSE liRelationID = TMSRelation.TMSRelationID.
         END.

         LEAVE.
      END.

      IF liRelationID > 0
      THEN DO:
         FIND TMSRelation EXCLUSIVE-LOCK WHERE
              TMSRelation.TMSRelationID = liRelationID.
         TMSRelation.ToTime = ADD-INTERVAL(ltzFromTime, -1, 'milliseconds').
      END.

      CREATE TMSRelation.
      ASSIGN
         TMSRelation.TableName    = icTableName
         TMSRelation.KeyType      = icKeyType
         TMSRelation.ChildValue   = icChildValue
         TMSRelation.ParentValue  = icParentValue
         TMSRelation.RelationType = icRelationType
         TMSRelation.FromTime     = ltzFromTime
         .

   END METHOD.

   METHOD PUBLIC STATIC VOID mEndRelation
      (icTableName    AS CHARACTER,
       icKeyType      AS CHARACTER,
       icParentValue  AS CHARACTER,
       icChildValue   AS CHARACTER,
       ilUseDate      AS LOGICAL):

      DEFINE BUFFER TMSRelation   FOR TMSRelation.
      DEFINE BUFFER lbTMSRelation FOR TMSRelation.

      DEFINE VARIABLE ltzToTime AS DATETIME-TZ NO-UNDO.

      IF ilUseDate
      THEN ltzToTime = DATETIME-TZ(TODAY, 0).
      ELSE ltzToTime = NOW.

      FOR EACH TMSRelation NO-LOCK WHERE
                TMSRelation.TableName   = icTableName   AND
                TMSRelation.KeyType     = icKeyType     AND
                TMSRelation.ChildValue  = icChildValue  AND
                TMSRelation.ParentValue = icParentValue
                BY TMSRelation.ToTime DESCENDING:
         IF TMSRelation.ToTime = ? OR TMSRelation.ToTime >= ltzToTime
         THEN DO TRANSACTION:
            FIND lbTMSRelation EXCLUSIVE-LOCK WHERE ROWID(lbTMSRelation) = ROWID(TMSRelation).
            lbTMSRelation.ToTime = ADD-INTERVAL(ltzToTime, -1, 'milliseconds').
         END.
         LEAVE.
      END.

   END METHOD.

END CLASS.