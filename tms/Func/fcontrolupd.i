/* fcontrolupd.i     15.11.07/aam

control user's update session

*/


/* how long will a keystroke be waited (seconds) */
DEF VAR liWaitKey     AS INT  NO-UNDO INIT 420.
/* how long a message is shown to user (ability to continue normal working) */
DEF VAR liWaitMessage AS INT  NO-UNDO INIT 180.
/* message that is shown to user */
DEF VAR lcWaitMessage AS CHAR NO-UNDO INIT
   "Too long transaction, cancelling .. (Press any key to continue working)".


/** basic scenario:

UPDATE xxx EDITING:

   READKEY PAUSE liWaitKey.

   IF LASTKEY = -1 AND TRANSACTION THEN DO:
      
      PAUSE 0.
      MESSAGE lcWaitMessage.
      PAUSE liWaitMessage NO-MESSAGE.
      
      IF LASTKEY NE -1 THEN NEXT.
      
      /* undo -> don't accept changes without possible validations and 
         without user knowing it */
      UNDO xxLoop, LEAVE xxLoop.
   END.
 
   APPLY LASTKEY.
END.


**/

