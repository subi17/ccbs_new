 /*------------------------------------------------------------------------
    File        : JsonToTT
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Mar 22 19:09:56 EET 2017
    Notes       : 

   Example use of the class with following json data:
{
  "msisdn": "62262256",
  "barrings": [{"barring_id": "Debt_LP", "value": "TRUE"}, {"barring_id": "Debt_LP", "value": "TRUE"}, {"barring_id": "Debt_LP", "value": "TRUE"}],
  "single_fees": [{"single_fee_id": "SINGLEFEE1", "amount": 4.55}, {"single_fee_id": "SINGLEFEE1", "amount": 4.55}],
  "upsells": [{"upsell_id": "DATA6_UPSELL", "amount": 500},{"upsell_id": "DATA6_UPSELL", "amount": 500}],
  "used_data_amount": 444,
  "terminals": ["puhelin", "telefooni"],
  "discounts": [{"discount_id": "discount1", "begin_date": "2017-03-01"}, {"discount_id": "discount1", "begin_date": "2017-03-01"}],
  "bonos": ["Bono_U2"],
  "saldo": 400,
  "fat": 411
}

DEFINE TEMP-TABLE ttBarrings NO-UNDO SERIALIZE-NAME ""
   FIELD barring_id AS CHARACTER
   FIELD barring_value AS CHARACTER SERIALIZE-NAME "value".

DEFINE TEMP-TABLE ttSingleFees NO-UNDO SERIALIZE-NAME ""
   FIELD single_fee_id AS CHARACTER
   FIELD amount AS DECIMAL.

DEFINE TEMP-TABLE ttUpsells NO-UNDO SERIALIZE-NAME ""
   FIELD upsell_id AS CHARACTER
   FIELD amount AS INTEGER.

DEFINE TEMP-TABLE ttDiscounts NO-UNDO SERIALIZE-NAME ""
   FIELD discount_id AS CHARACTER
   FIELD begin_date AS DATE.

DEFINE TEMP-TABLE ttRootlevel NO-UNDO SERIALIZE-NAME ""
   FIELD msisdn           AS CHARACTER
   FIELD used_data_amount AS INTEGER
   FIELD saldo            AS INTEGER
   FIELD fat              AS INTEGER.

DEFINE VARIABLE objJsonToTT AS CLASS Class.JsonToTT.

objJsonToTT = NEW Class.JsonToTT().

objJsonToTT:mStoreTT(BUFFER ttRootLevel:HANDLE).

objJsonToTT:mStoreTT("barrings", BUFFER ttBarrings:HANDLE).
objJsonToTT:mStoreTT("single_fees", BUFFER ttSingleFees:HANDLE).
objJsonToTT:mStoreTT("upsells", BUFFER ttUpsells:HANDLE).
objJsonToTT:mStoreTT("discounts", BUFFER ttDiscounts:HANDLE).

objJsonToTT:mParseJsonFile("/tmp/jsondata.json").
DO WHILE objJsonToTT:mGetNext():

   FOR EACH ttRootLevel: DISP ttRootLevel. END.
   FOR EACH ttBarrings: DISP ttBarrings. END.
   FOR EACH ttServices: DISP ttServices. END.
   FOR EACH ttSinglefees: DISP ttSinglefees. END.
   FOR EACH ttUpsells: DISP ttUpsells. END.
   FOR EACH ttDiscounts: DISP ttDiscounts. END.

   MESSAGE objJsonToTT:mListJsonArrayNames() VIEW-AS ALERT-BOX TITLE "Arrays".
   MESSAGE objJsonToTT:mListJsonObjectNames() VIEW-AS ALERT-BOX TITLE "Objects".

END.

FINALLY:
   IF VALID-OBJECT(objJsonToTT)
   THEN DELETE OBJECT objJsonToTT.
END FINALLY.
  
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

&GLOBAL-DEFINE MAINLEVELNAME CHR(1) + "MainLevel" 

CLASS Class.JsonToTT: 

   DEFINE PRIVATE VARIABLE objJsonConstruct   AS CLASS JsonConstruct    NO-UNDO.
   DEFINE PRIVATE VARIABLE objJsonArray       AS CLASS JsonArray        NO-UNDO.
   DEFINE PRIVATE VARIABLE lcMainLevelFields  AS CHARACTER              NO-UNDO.
   DEFINE PRIVATE VARIABLE liArrayLength      AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE liCurrentItem      AS INTEGER INITIAL 1      NO-UNDO.

   DEFINE PRIVATE TEMP-TABLE ttJsonTempTable NO-UNDO
      FIELD jsonentry     AS CHARACTER
      FIELD temptableHandle AS HANDLE
      INDEX jsonentry IS PRIMARY UNIQUE jsonentry
      .

   DEFINE PRIVATE TEMP-TABLE ttJsonArray NO-UNDO
      FIELD jsonarrname    AS CHARACTER
      FIELD jsonarr        AS CLASS Progress.Lang.Object
      INDEX jsonarrname IS PRIMARY UNIQUE jsonarrname
      .

   DEFINE PRIVATE TEMP-TABLE ttJsonObject NO-UNDO
      FIELD jsonobjname     AS CHARACTER
      FIELD jsonobj         AS CLASS Progress.Lang.Object
      INDEX jsonobjname IS PRIMARY UNIQUE jsonobjname
      .

   DESTRUCTOR JsonToTT():

      mEmptyArray().

      EMPTY TEMP-TABLE ttJsonTempTable.

   END DESTRUCTOR.


   METHOD PUBLIC LOGICAL mGetNext():

      mEmptyCurrentData().

      IF liCurrentItem > liArrayLength
      THEN RETURN FALSE.

      mParse(liCurrentItem).

      liCurrentItem = liCurrentItem + 1.

      RETURN TRUE.

   END METHOD.


   METHOD PRIVATE VOID mParseToJsonArray
      ( icData AS LONGCHAR,
        icType AS CHARACTER ):

      mEmptyArray().

      DEFINE VARIABLE objOMParser AS CLASS ObjectModelParser NO-UNDO.

      objOMParser = NEW ObjectModelParser().

      CASE icType:
         WHEN "FILE" THEN objJsonConstruct = objOMParser:ParseFile(STRING(icData)).
         WHEN "DATA" THEN objJsonConstruct = objOMParser:Parse(icData).
      END CASE.

      IF objJsonConstruct:getClass():TypeName = "Progress.Json.ObjectModel.JsonArray"
      THEN ASSIGN
              objJsonArray  = CAST(objJsonConstruct, JsonArray)
              liArrayLength = objJsonArray:Length.
      ELSE DO:
         objJsonArray  = NEW JsonArray().
         objJsonArray:Add(CAST(objJsonConstruct, JsonObject)).
      END.

      FINALLY:
         IF VALID-OBJECT(objOMParser)
         THEN DELETE OBJECT objOMParser.
      END FINALLY.

   END METHOD.


   METHOD PUBLIC VOID mParseJsonFile
      (icJsonFile AS CHARACTER):

      mParseToJsonArray(icJsonFile, "FILE").

   END METHOD.


   METHOD PUBLIC VOID mParseJsonData
      (icJsonData AS LONGCHAR):

      mParseToJsonArray(icJsonData, "DATA").

   END METHOD.


   METHOD PUBLIC VOID mParseJsonObject
      (iobjJsonObject AS CLASS JsonObject):

      mEmptyArray().

      objJsonArray  = NEW JsonArray().
      objJsonArray:Add(iobjJsonObject).

   END METHOD.


   METHOD PRIVATE VOID mEmptyArray():

      mEmptyCurrentData().

      ASSIGN
         liArrayLength = 1
         liCurrentItem = 1.

      IF VALID-OBJECT(objJsonConstruct)
      THEN DELETE OBJECT objJsonConstruct.

      IF VALID-OBJECT(objJsonArray)
      THEN DELETE OBJECT objJsonArray.

   END METHOD.


   METHOD PRIVATE VOID mEmptyCurrentData():

      FOR EACH ttJsonTempTable:
         IF VALID-HANDLE(ttJsonTempTable.temptableHandle)
         THEN ttJsonTempTable.temptableHandle:EMPTY-TEMP-TABLE().
      END.

      EMPTY TEMP-TABLE ttJsonArray.
      EMPTY TEMP-TABLE ttJsonObject.
      
   END METHOD.



   METHOD PUBLIC VOID mStoreTT
      ( icJsonEntry AS CHARACTER,
        ihTTHandle  AS HANDLE ):

      CREATE ttJsonTempTable.
      ASSIGN
         ttJsonTempTable.jsonentry       = icJsonEntry
         ttJsonTempTable.temptableHandle = ihTTHandle
         . 

   END METHOD.


   METHOD PUBLIC VOID mStoreTT
      ( ihTTHandle  AS HANDLE ):

      mStoreTT({&MAINLEVELNAME}, ihTTHandle).

      DEFINE VARIABLE lii AS INTEGER NO-UNDO.
      
      DO lii = 1 TO ihTTHandle:NUM-FIELDS:
         lcMainLevelFields = lcMainLevelFields + "," +
                             ihTTHandle:BUFFER-FIELD(lii):SERIALIZE-NAME.
      END.

      lcMainLevelFields = SUBSTRING(lcMainLevelFields, 2).

   END METHOD.   

   
   METHOD PRIVATE VOID mJsonObjectToTT
      ( ihHandle AS HANDLE,
        iclJson AS LONGCHAR ):

      DEFINE VARIABLE lcJson AS LONGCHAR INITIAL '~{"": &1}' NO-UNDO.
      lcJson = SUBSTITUTE(lcJson, iclJson).
      
      ihHandle:READ-JSON("LONGCHAR",lcJson,"EMPTY").

   END METHOD.


   METHOD PRIVATE VOID mStoreJsonArray
      ( icArrayName AS CHARACTER,
        iobjArray   AS CLASS JsonArray ):

      CREATE ttJsonArray.
   
      ASSIGN
         ttJsonArray.jsonarrname = icArrayName
         ttJsonArray.jsonarr     = iobjArray
         .

   END METHOD.


   METHOD PRIVATE VOID mStoreJsonObject
      ( icObjectName AS CHARACTER,
        iobjObject   AS CLASS JsonObject ):

      CREATE ttJsonObject.
   
      ASSIGN
         ttJsonObject.jsonobjname = icObjectName
         ttJsonObject.jsonobj     = iobjObject
         .

   END METHOD.


   METHOD PUBLIC CHARACTER mListJsonArrayNames():
      
      DEFINE VARIABLE lcNames AS CHARACTER NO-UNDO.

      FOR EACH ttJsonArray:
         lcNames = lcNames + "," + ttJsonArray.jsonarrname.         
      END.

      IF lcNames > ""
      THEN RETURN SUBSTRING(lcNames,2).
      
      RETURN "".

   END METHOD.


   METHOD PUBLIC CHARACTER mListJsonObjectNames():
      
      DEFINE VARIABLE lcNames AS CHARACTER NO-UNDO.

      FOR EACH ttJsonObject:
         lcNames = lcNames + "," + ttJsonObject.jsonobjname.         
      END.
   
      IF lcNames > ""
      THEN RETURN SUBSTRING(lcNames,2).
      
      RETURN "".

   END METHOD.


   METHOD PUBLIC JsonArray mGetJsonArray
      ( icArrayName AS CHARACTER ):

      FIND ttJsonArray WHERE ttJsonArray.jsonarrname = icArrayName NO-ERROR.
      IF NOT AVAILABLE ttJsonArray
      THEN RETURN ERROR NEW Progress.Lang.AppError("JsonArray with name " + icArrayName + " doesn't exist.", 1).

      RETURN CAST(ttJsonArray.jsonarr, JsonArray).
      
   END METHOD.


   METHOD PUBLIC JsonObject mGetJsonObject
      ( icObjectName AS CHARACTER ):

      FIND ttJsonObject WHERE ttJsonObject.jsonobjname = icObjectName NO-ERROR.
      IF NOT AVAILABLE ttJsonObject
      THEN RETURN ERROR NEW Progress.Lang.AppError("JsonObject with name " + icObjectName + " doesn't exist.", 1).

      RETURN CAST(ttJsonObject.jsonobj, JsonObject).
      
   END METHOD.


   METHOD PRIVATE VOID mParse
      ( iiItem AS INTEGER ):

      DEFINE VARIABLE lcField       AS CHARACTER EXTENT  NO-UNDO.
      DEFINE VARIABLE lii           AS INTEGER           NO-UNDO.
      DEFINE VARIABLE objJsonObject AS CLASS JsonObject  NO-UNDO.

      ASSIGN
         objJsonObject = objJsonArray:GetJsonObject(iiItem)
         lcField       = objJsonObject:getNames().

      /* 
         First we will go through the fields in json data 
         and process the data which is not possible to store
         directly to the "main" (root level) temp table.
         
         If the field has a same name what has been stored to the
         temp-table names then we will read data to the temp table
         from the field.
         
         If the field name is not among with the stored temp-table
         names and the field contains either JsonArray or JsonObject
         type data then we store the field to ttJsonArray or
         ttJsonObject respectively.
         
         Otherwise we can store the data to the "main" temp table. Which
         we will do in next step.
      */
      FIELD_LOOP:  
      DO lii = 1 TO EXTENT(lcField): 
         FOR ttJsonTempTable WHERE
            ttJsonTempTable.jsonentry = lcField[lii]:
            
            mJsonObjectToTT(ttJsonTempTable.temptableHandle, objJsonObject:GetJsonText(lcField[lii])).
            objJsonObject:Remove(lcField[lii]).
            
            NEXT FIELD_LOOP.
         END.

         IF objJsonObject:GetType(lcField[lii]) = JsonDataType:ARRAY
         THEN DO:
            mStoreJsonArray(lcField[lii], objJsonObject:GetJsonArray(lcField[lii])).
            objJsonObject:Remove(lcField[lii]).
            NEXT FIELD_LOOP.
         END.
      
         IF objJsonObject:GetType(lcField[lii]) = JsonDataType:OBJECT
         THEN DO:
            mStoreJsonObject(lcField[lii], objJsonObject:GetJsonObject(lcField[lii])).
            objJsonObject:Remove(lcField[lii]).
            NEXT FIELD_LOOP.
         END.
         
         IF LOOKUP(lcField[lii],lcMainLevelFields) = 0
         THEN RETURN ERROR NEW Progress.Lang.AppError("The main (root) level temp-table cannot handle field " + lcField[lii], 1).

      END.
      
      /* Lets store the remaining json data to the "main" temp-table. */
      FOR ttJsonTempTable WHERE
         ttJsonTempTable.jsonentry = {&MAINLEVELNAME}:
         
         mJsonObjectToTT(ttJsonTempTable.temptableHandle, objJsonObject:GetJsonText()).
      END.

      FINALLY:

         EXTENT(lcField) = ?. 

      END FINALLY.   
   
   END METHOD.

END CLASS.
