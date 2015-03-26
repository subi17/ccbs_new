/* ecgleave    18.03.03/aam
   test leave task of ecg member 
*/


DEF INPUT  PARAMETER icGroup    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiCustNum  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocResult   AS CHAR NO-UNDO.


MESSAGE "Leaving task being run for member" iiCustNum 
        "that is removed from group" icGroup
VIEW-AS ALERT-BOX.

ocResult = "OK".

