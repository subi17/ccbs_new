ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.HdrTextModel IMPLEMENTS Gwy.ModelInterface:

DEF VAR lJson          AS LONGCHAR NO-UNDO.
DEF VAR lModelid       AS CHAR NO-UNDO.
DEF VAR lCorrelationID AS CHAR NO-UNDO.
DEF VAR gcBrand        AS CHAR NO-UNDO.
DEF VAR loRequestMaker AS Gwy.RequestMaker NO-UNDO.
DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    
DEFINE TEMP-TABLE ttHdrText NO-UNDO SERIALIZE-NAME "HdrText"
   FIELD external_id AS CHAR.
    
DEFINE TEMP-TABLE ttTextItem NO-UNDO SERIALIZE-NAME "billing_text"
   FIELD parent_id AS RECID 
   FIELD language AS CHAR
   FIELD bill_text AS CHAR
   FIELD ti_language AS INT.
   
DEFINE DATASET HdrTextDataSet FOR ttHdrText, ttTextItem
   PARENT-ID-RELATION idparent FOR ttHdrText, ttTextItem
      PARENT-ID-FIELD parent_id.
 

CONSTRUCTOR PUBLIC HdrTextModel ( iJson    AS LONGCHAR,
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

   DEF VAR liText  AS INT  NO-UNDO. 
   DEF VAR lcError AS CHAR NO-UNDO.
 
   lJson = '~{"HdrTextDataSet":~{"HdrText":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET HdrTextDataset BIND, lJson) THEN
       RETURN FALSE.
   
   FIND ttHdrText NO-ERROR.
   IF NOT AVAILABLE ttHdrText THEN
       RETURN loRequestMaker:logError("Empty HdrText set").

   IF ttHdrText.external_id = "" THEN 
      RETURN loRequestMaker:logError("Invalid HdrText ID").

   liText = INT(ttHdrText.external_id) NO-ERROR.
   IF liText = 0 THEN 
      RETURN loRequestMaker:logError("Invalid HdrText ID").
        
   lcError = validate_items(RECID(ttHdrText),
                            liText,
                            TRUE).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   FOR EACH ttTextItem WHERE
            ttTextItem.parent_id = RECID(ttHdrText):
      CREATE HdrText.
      ASSIGN 
         HdrText.Brand  = gcBrand
         HdrText.te-nro = liText
         HdrText.te-kie = ttTextItem.ti_language
         HdrText.te-text = ttTextItem.bill_text.

      loEventLogMaker:make_eventlog("create",BUFFER HdrText:HANDLE).
      
      RELEASE HdrText.
   END.
   
   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("HdrText",
                                  ttHdrText.external_id,
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("HdrText create: Id=&1 Ext.Request=&2", 
                 ttHdrText.external_id,
                 lCorrelationID), 
      "DEBUG").
 
END METHOD.

METHOD PUBLIC LOG update():
  
   DEF VAR liText  AS INT  NO-UNDO. 
   DEF VAR lcError AS CHAR NO-UNDO.
   DEF VAR llDone  AS LOG  NO-UNDO.
   DEF VAR liWait  AS INT  NO-UNDO.
   
   IF lModelid = ? THEN 
      RETURN loRequestMaker:logError('HdrText ID not given').

   lJson = '~{"HdrTextDataSet":~{"HdrText":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET HdrTextDataset BIND, lJson) THEN
       RETURN FALSE.
   
   IF lModelID = "" THEN 
      RETURN loRequestMaker:logError("Invalid HdrText ID").
      
   FIND ttHdrText NO-ERROR.
   IF NOT AVAILABLE ttHdrText THEN
       RETURN loRequestMaker:logError("Empty HdrText set").

   liText = INT(lModelid) NO-ERROR.
   IF liText = 0 THEN 
      RETURN loRequestMaker:logError("Invalid HdrText ID").
        
   lcError = validate_items(RECID(ttHdrText),
                            liText,
                            FALSE).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   llDone = FALSE.
   FOR EACH ttTextItem WHERE
            ttTextItem.parent_id = RECID(ttHdrText):
       
      liWait = 0.
      DO WHILE TRUE:
         FIND FIRST HdrText WHERE 
                    HdrText.Brand = gcBrand AND
                    HdrText.te-nro = liText AND
                    HdrText.te-kie = ttTextItem.ti_language 
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         IF LOCKED(HdrText) THEN DO:
            liWait = liWait + 1.
            /* some texts may have already been updated but the entire update
               can be resent */
            IF liWait > {&WAIT_CONFIG_LOCK} THEN 
               RETURN loRequestMaker:logError("Text was not available for update").
            PAUSE 1 NO-MESSAGE. 
            NEXT. 
         END.
         LEAVE.
      END.
     
      IF AVAILABLE HdrText THEN DO:
      
         loEventLogMaker:make_eventlog("oldbuffer",BUFFER HdrText:HANDLE).

         HdrText.te-text = ttTextItem.bill_text.
   
         loEventLogMaker:make_eventlog("modify",BUFFER HdrText:HANDLE).
   
         RELEASE HdrText.
         llDone = TRUE.
      END.    
   END.
   
   /* error cases get response via logError, make response for 
      the succesful one */
   IF llDone THEN 
      loRequestMaker:create_response("HdrText",
                                     lModelID,
                                     "OK",
                                     ""). 
   ELSE RETURN loRequestMaker:logError("Nothing updated").
   
   loRequestMaker:create_log(
      SUBSTITUTE("HdrText update: Id=&1 Ext.Request=&2", 
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

METHOD PUBLIC CHAR validate_items
   (irParentID AS RECID,
    iiText   AS INT,
    ilCreate AS LOG):
 
   FOR EACH ttTextItem WHERE
            ttTextItem.parent_id = irParentID:

      FIND FIRST Language WHERE 
         Language.IsoCode = ttTextItem.language NO-LOCK NO-ERROR.
      IF AVAILABLE Language THEN ttTextItem.ti_language = Language.Language.
      ELSE RETURN "Invalid language". 
            
      IF CAN-FIND(FIRST HdrText WHERE 
                        HdrText.Brand = gcBrand AND
                        HdrText.te-nro = iiText AND
                        HdrText.te-kie = ttTextItem.ti_language) THEN DO:
         IF ilCreate THEN RETURN "Billing text already exists".
      END.   
      ELSE IF NOT ilCreate THEN RETURN "Unknown billing text".
   END.
   
   RETURN "".
 
END METHOD.

METHOD PUBLIC LOG process_other_update():

END METHOD.

DESTRUCTOR PUBLIC HdrTextModel ():
   IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
   IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
END DESTRUCTOR.

END CLASS.

