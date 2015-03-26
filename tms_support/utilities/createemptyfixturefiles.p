DEFINE VARIABLE cDir AS CHARACTER NO-UNDO. 
cDir = "/data/fixtures/emptyfixtures/".

DEFINE STREAM sFix.

FUNCTION fProcessTable RETURN LOGICAL (INPUT pcDbName AS CHARACTER,
   INPUT pcTableName AS CHARACTER):

   OUTPUT STREAM sFix TO VALUE(cDir + pcTableName + ".yaml").
   PUT STREAM sFix UNFORMATTED " " SKIP.
   OUTPUT STREAM sFix CLOSE.
   
   RETURN TRUE.
END.




FOR EACH common._FILE NO-LOCK:
     fProcessTable("common", common._FILE._File-name).
END.


FOR EACH mobile._FILE NO-LOCK:
     fProcessTable("mobile", mobile._FILE._File-name).
END.


FOR EACH mcdr._FILE NO-LOCK:
     fProcessTable("mcdr", mcdr._FILE._File-name).
END.


FOR EACH mcdrdtl._FILE NO-LOCK:
     fProcessTable("mcdrdtl", mcdrdtl._FILE._File-name).
END.


FOR EACH prepcdr._FILE NO-LOCK:
     fProcessTable("prepcdr", prepcdr._FILE._File-name).
END.


FOR EACH roamcdr._FILE NO-LOCK:
     fProcessTable("roamcdr", roamcdr._FILE._File-name).
END.


FOR EACH ordercanal._FILE NO-LOCK:
     fProcessTable("ordercanal", ordercanal._FILE._File-name).
END.
