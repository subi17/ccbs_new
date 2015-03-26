/* farplog.i      12.08.02/aam 
   log for payment files 

                  09.09.03/aam brand
*/

DEF VAR lcDouble  AS CHAR NO-UNDO. 
DEF VAR llConfirm AS LOGIC NO-UNDO. 

/* check if same PaymFile is being rerun */
FUNCTION fCheckArplog RETURNS LOGICAL
   (icFile   AS CHAR,
    icPrefix AS CHAR,
    icUser   AS CHAR). 

   /* get the plain file-name */
   IF R-INDEX(icFile,"/") GT 0
   THEN ASSIGN icFile = SUBSTRING(icFile,R-INDEX(icFile,"/") + 1). 

   /* same file being rerun */
   FIND FIRST PaymLog NO-LOCK WHERE
      PaymLog.Brand    = gcBrand AND
      PaymLog.PaymFile = icPrefix + icFile NO-ERROR.
   IF AVAILABLE PaymLog THEN DO:

      llConfirm = FALSE. 

      MESSAGE "File" icFile "has already been Booked to TMS on"
              PaymLog.BookDate "." SKIP
              "Do You want to book the same file again ?" SKIP
              "WARNING: Result may be unwanted double bookings."
      VIEW-AS ALERT-BOX
      QUESTION
      BUTTONS YES-NO
      TITLE " Rereading of file "
      SET llConfirm.

      IF NOT llConfirm THEN RETURN FALSE. 
   END. 

   RETURN TRUE. 

END FUNCTION.

FUNCTION fCreateArplog RETURNS LOGICAL
   (icFile   AS CHAR,
    icPrefix AS CHAR,
    icUser   AS CHAR). 

   /* get the plain file-name */
   IF R-INDEX(icFile,"/") GT 0
   THEN ASSIGN icFile = SUBSTRING(icFile,R-INDEX(icFile,"/") + 1). 

   lcDouble = "". 

   /* same PaymFile being rerun */
   IF CAN-FIND(FIRST PaymLog WHERE
                     PaymLog.Brand    = gcBrand AND
                     PaymLog.PaymFile = icPrefix + icFile)
   THEN ASSIGN lcDouble = ":Rerun" + STRING(TIME).

   /* create a log entry */
   CREATE PaymLog.
   ASSIGN
   PaymLog.Brand    = gcBrand
   PaymLog.PaymFile = icPrefix + icFile + lcDouble
   PaymLog.UserCode = icUser
   PaymLog.BookDate = TODAY.

   RETURN TRUE. 

END FUNCTION. 


