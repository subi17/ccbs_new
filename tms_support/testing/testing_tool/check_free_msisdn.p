{Func/timestamp.i}

DEF VAR lcEmaSelection  AS CHAR NO-UNDO FORMAT "X(6)".
DEF VAR MSISDN_status   AS INT  NO-UNDO FORMAT "Z9".

FORM
   SKIP
   "Normal or EMA MSISDN search (default:Normal):" lcEmaSelection SKIP
   WITH OVERLAY CENTERED ROW 10 TITLE " Check Free MSISDN " NO-LABELS
   FRAME fclean.

UPDATE lcEmaSelection with FRAME fclean.

/* EMA status is 98 and others 99 */
IF lcEmaSelection = "EMA" THEN MSISDN_status = 98.
ELSE MSISDN_status = 99.   /* use normal status value */

FOR EACH MSISDN NO-LOCK WHERE
         MSISDN.Brand = "1" AND
         MSISDN.ValidTo GE fMakeTS() AND
         MSISDN.StatusCode EQ MSISDN_status :
DISP MSISDN.CLI.
END.