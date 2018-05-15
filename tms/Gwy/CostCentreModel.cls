ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

&GLOBAL-DEFINE WAIT_CONFIG_LOCK 20.

{Syst/tmsconst.i}

CLASS Gwy.CostCentreModel IMPLEMENTS Gwy.ModelInterface:

DEF VAR lJson          AS LONGCHAR NO-UNDO.
DEF VAR lModelid       AS CHAR NO-UNDO.
DEF VAR lCorrelationID AS CHAR NO-UNDO.
DEF VAR gcBrand        AS CHAR NO-UNDO.
DEF VAR loRequestMaker AS Gwy.RequestMaker NO-UNDO.
DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    
DEFINE TEMP-TABLE ttCostCentre NO-UNDO SERIALIZE-NAME "CostCentre"
   FIELD external_id AS CHAR 
   FIELD name AS CHAR.
   
DEFINE DATASET CostCentreDataSet FOR ttCostCentre.    

CONSTRUCTOR PUBLIC CostCentreModel ( iJson    AS LONGCHAR,
                                  iModelid AS CHAR, 
                                  iSource  AS CHAR,
                                  iCorrelationID AS CHAR,
                                  iReplyTo AS CHAR):

   ASSIGN
      gcBrand        = "1"
      lJson          = iJson
      lModelid       = iModelid
      lCorrelationID = iCorrelationID
      loRequestMaker = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
      loEventLogMaker = NEW Gwy.EventLogMaker(iSource).
   
END CONSTRUCTOR.

METHOD PUBLIC LOG create():
 
   lJson = '~{"CostCentreDataSet":~{"CostCentre":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET CostCentreDataset BIND, lJson) THEN
       RETURN FALSE.
   
   FIND ttCostCentre NO-ERROR.
   IF NOT AVAILABLE ttCostCentre THEN
       RETURN loRequestMaker:logError("Empty CostCentre set").

   IF ttCostCentre.external_id = "" THEN 
      RETURN loRequestMaker:logError("Invalid CostCentre ID").
        
   IF CAN-FIND(FIRST CostCentre WHERE 
                     CostCentre.Brand = gcBrand AND
                     CostCentre.CostCentre = ttCostCentre.external_id) THEN 
      RETURN loRequestMaker:logError("CostCentre already exists").
      
   CREATE CostCentre.
   ASSIGN 
      CostCentre.Brand  = gcBrand
      CostCentre.CostCentre = ttCostCentre.external_id
      CostCentre.CCName = ttCostCentre.name.

   loEventLogMaker:make_eventlog("create",BUFFER CostCentre:HANDLE).
      
   RELEASE CostCentre.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("CostCentre",
                                  ttCostCentre.external_id,
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("CostCentre create: Id=&1 Ext.Request=&2", 
                 ttCostCentre.external_id,
                 lCorrelationID), 
      "DEBUG").
 
END METHOD.

METHOD PUBLIC LOG update():

   DEF VAR liWait AS INT  NO-UNDO.
   
   IF lModelid = ? THEN 
      RETURN loRequestMaker:logError('CostCentre ID not given').

   lJson = '~{"CostCentreDataSet":~{"CostCentre":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET CostCentreDataset BIND, lJson) THEN
       RETURN FALSE.
   
   IF lModelID = "" THEN 
      RETURN loRequestMaker:logError("Invalid CostCentre ID").
      
   FIND ttCostCentre NO-ERROR.
   IF NOT AVAILABLE ttCostCentre THEN
       RETURN loRequestMaker:logError("Empty CostCentre set").
   
   DO WHILE TRUE:
      FIND FIRST CostCentre WHERE 
                 CostCentre.Brand = gcBrand AND
                 CostCentre.CostCentre = lModelID EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF LOCKED(CostCentre) THEN DO:
         liWait = liWait + 1.
         IF liWait > "{&WAIT_CONFIG_LOCK}" THEN 
            RETURN loRequestMaker:logError("Cost center was not available for update").
         PAUSE 1 NO-MESSAGE. 
         NEXT. 
      END.
      LEAVE.
   END.

   IF NOT AVAILABLE CostCentre THEN 
      RETURN loRequestMaker:logError("Unknown CostCentre").
      
   loEventLogMaker:make_eventlog("oldbuffer",BUFFER CostCentre:HANDLE).

   IF ttCostCentre.name > "" THEN CostCentre.CCName = ttCostCentre.name.
   
   loEventLogMaker:make_eventlog("modify",BUFFER CostCentre:HANDLE).
   
   RELEASE CostCentre.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("CostCentre",
                                  lModelID,
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("CostCentre update: Id=&1 Ext.Request=&2", 
                 lModelID,
                 lCorrelationID), 
      "DEBUG").
       
END METHOD.

METHOD PUBLIC LOG makeOtherAction(icAction AS CHAR):
   CASE icAction:
      WHEN "update_other" THEN process_other_update().
      OTHERWISE 
         RETURN loRequestMaker:logError(SUBST("Unknown action &1",icAction)).
   END CASE.
END METHOD.

METHOD PUBLIC LOG process_other_update():

END METHOD.

DESTRUCTOR PUBLIC CostCentreModel ():
   IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
   IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
END DESTRUCTOR.

END CLASS.

