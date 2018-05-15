ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS Gwy.RepTextMaker:

DEF VAR gcBrand        AS CHAR NO-UNDO.
DEF VAR loRequestMaker AS Gwy.RequestMaker NO-UNDO.
DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
DEF VAR lhFind         AS HANDLE NO-UNDO.
DEF VAR lhText         AS HANDLE NO-UNDO.
    
CONSTRUCTOR PUBLIC RepTextMaker (iSource AS CHAR):
   ASSIGN
      gcBrand         = "1"
      loRequestMaker  = NEW Gwy.RequestMaker("", "", iSource)
      loEventLogMaker = NEW Gwy.EventLogMaker(iSource).
   
END CONSTRUCTOR.


METHOD PUBLIC CHAR validate_reptext 
   (INPUT TABLE-HANDLE ihTable BIND,
    iiTextType AS INT,
    icLinkCode AS CHAR, 
    irParentID AS RECID):
    
   DEF VAR lcResult AS CHAR NO-UNDO.
   

   CREATE BUFFER lhText FOR TABLE ihTable.

   CREATE QUERY lhFind.
   lhFind:SET-BUFFERS(lhText).
   
   lhFind:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 WHERE &1.&2 = &3",
                                   ihTable:NAME,
                                   "parent_id",
                                   irParentID)).
   lhFind:QUERY-OPEN.

   REPEAT:
      lhFind:GET-NEXT.
      IF lhFind:QUERY-OFF-END THEN LEAVE.

      ASSIGN 
         lhText::rt_validfrom = loRequestMaker:make_date(
                                   loRequestMaker:make_timestamp(lhText::valid_from))
         lhText::rt_validto  = loRequestMaker:make_date(
                                   loRequestMaker:make_timestamp(lhText::valid_to))
         NO-ERROR.
      IF lhText::rt_validfrom = ? OR lhText::rt_validto = ? THEN DO:
         lcResult = "Invalid commercial name period". 
         LEAVE.
      END.
         
      FIND FIRST Language WHERE 
         Language.LanguageCode = lhText::language NO-LOCK NO-ERROR.
      IF AVAILABLE Language THEN lhText::rt_language = Language.Language.
      ELSE DO:
         lcResult = "Invalid commercial name language". 
         LEAVE.
      END.

      IF CAN-FIND(FIRST RepText WHERE
                        RepText.Brand    = gcBrand AND
                        RepText.TextType = iiTextType AND
                        RepText.LinkCode = icLinkCode AND
                        RepText.Language = lhText::rt_language AND
                        RepText.FromDate <= lhText::rt_validto AND
                        RepText.ToDate   >= lhText::rt_validfrom AND
                        RepText.FromDate NE lhText::rt_validfrom) THEN DO:
         lcResult = "Commercial name with overlapping period exists".
         LEAVE.
      END.

      IF CAN-FIND(FIRST RepText WHERE 
                        RepText.Brand    = gcBrand AND
                        RepText.TextType = iiTextType AND
                        RepText.LinkCode = icLinkCode AND
                        RepText.Language = lhText::rt_language AND
                        RepText.ToDate   = lhText::rt_validto AND
                        RepText.FromDate NE lhText::rt_validfrom) THEN DO:
         lcResult = "Commercial name period end date is already in use".
         LEAVE.
      END.
   END.

   lhFind:QUERY-CLOSE.
   
   DELETE OBJECT lhFind.
   DELETE OBJECT lhText.
 
   RETURN lcResult. 
 
END METHOD.

METHOD PUBLIC CHAR update_reptext 
   (INPUT TABLE-HANDLE ihTable BIND,
    iiTextType AS INT,
    icLinkCode AS CHAR, 
    irParentID AS RECID):

   DEF VAR liWait AS INT   NO-UNDO.
   DEF VAR lrText AS RECID NO-UNDO.


   CREATE BUFFER lhText FOR TABLE ihTable.

   CREATE QUERY lhFind.
   lhFind:SET-BUFFERS(lhText).
   
   lhFind:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 WHERE &1.&2 = &3",
                                   ihTable:NAME,
                                   "parent_id",
                                   irParentID)).
   lhFind:QUERY-OPEN.

   REPEAT:
      lhFind:GET-NEXT.
      IF lhFind:QUERY-OFF-END THEN LEAVE.

      /* if exists with given attributes then just update */
      FIND FIRST RepText WHERE
                 RepText.Brand    = gcBrand AND
                 RepText.TextType = iiTextType AND
                 RepText.LinkCode = icLinkCode AND
                 RepText.Language = lhText::rt_language AND
                 RepText.FromDate = lhText::rt_validfrom 
         NO-LOCK NO-ERROR.

      IF AVAILABLE RepText THEN DO: 
         ASSIGN 
            liWait = 0
            lrText = RECID(RepText).
      
         DO WHILE TRUE:
             /* 'find current' doesn't work with this, it doesn't get the record 
                when it's released, so use recid find  */
            FIND FIRST RepText WHERE RECID(RepText) = lrText
               EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED(RepText) THEN DO:
               liWait = liWait + 1.
               /* some texts and the main table may have already been updated but 
                  the entire update can be resent */
               IF liWait > {&WAIT_CONFIG_LOCK} THEN 
                  RETURN "Commercial name was not available for update".
               PAUSE 1 NO-MESSAGE. 
               NEXT. 
            END.
            LEAVE.
         END.

         loEventLogMaker:make_eventlog("oldbuffer",BUFFER RepText:HANDLE).
         ASSIGN 
            RepText.ToDate  = lhText::rt_validto
            RepText.RepText = lhText::comm_name.
         loEventLogMaker:make_eventlog("modify",BUFFER RepText:HANDLE).   
         RELEASE RepText.
      END.

      /* new entry */
      ELSE DO:
         CREATE RepText.
         ASSIGN
            RepText.Brand    = gcBrand 
            RepText.TextType = iiTextType
            RepText.LinkCode = icLinkCode
            RepText.Language = lhText::rt_language
            RepText.FromDate = lhText::rt_validfrom
            RepText.ToDate   = lhText::rt_validto
            RepText.RepText  = lhText::comm_name.
   
         loEventLogMaker:make_eventlog("create",BUFFER RepText:HANDLE).

         RELEASE RepText.
      END.   
   END.   

   lhFind:QUERY-CLOSE.
   
   DELETE OBJECT lhFind.
   DELETE OBJECT lhText.
   
   RETURN "". 
 
END METHOD. 

DESTRUCTOR PUBLIC RepTextMaker ():
   IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
   IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
END DESTRUCTOR.

END CLASS.

