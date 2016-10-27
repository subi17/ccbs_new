DEF VAR liCode AS INT NO-UNDO.
DEF VAR liNum1 AS INT NO-UNDO.
DEF VAR liNum2 AS INT NO-UNDO.

liNum1 = 9853.
liNum2 = 9854.
/*FOR EACH menutext NO-LOCK 
:
   DISP menutext.
END.
*/

/*
FOR EACH menutext NO-LOCK 
:
   if menutext.menunum > liCode THEN liCode = menutext.menunum.

END.

disp licode.
*/

FIND FIRST menutext NO-LOCK where menutext.menunum EQ liNum1 NO-ERROR.

IF NOT AVAIL menutext THEN DO:
CREATE menutext.
ASSIGN menutext.menutext = "SHOW    MESSAGES"
       menutext.menunum = linum1.

END.
ELSE message "already found " + string (liNum2) VIEW-AS ALERT-BOX.

RELEASE menutext.

FIND FIRST menutext NO-LOCK where menutext.menunum EQ liNum2 NO-ERROR.

IF NOT AVAIL menutext THEN DO:
CREATE menutext.
ASSIGN menutext.menutext = "INSTALL ADDRESS"
       menutext.menunum = linum2.

END.
ELSE message "already found " + string (liNum1) VIEW-AS ALERT-BOX.
