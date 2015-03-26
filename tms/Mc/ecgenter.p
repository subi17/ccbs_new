/* ecgenter    18.03.03/aam
   test enter task of ecg member 
*/


DEF INPUT  PARAMETER icGroup    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiCustNum  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocResult   AS CHAR NO-UNDO.


MESSAGE "Entering task being run for new member" iiCustNum 
        "in group" icGroup
VIEW-AS ALERT-BOX.

ocResult = "OK".

