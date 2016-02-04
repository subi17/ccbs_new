/* ---------------------------------------------------------------
  MODULE .......: NNOCRR.P
  FUNCTION .....: Build a stanrardized Postgiro OCR STRING              
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 23.04.97 pt
  CHANGED ......: 14.04.99 pt datatype ref:  DECIMAL
                  InvNum MAX 11 digits
  Version ......: M15
  --------------------------------------------------------------- */

DEF INPUT PARAMETER  InvNum   AS DE.   /* MAX 9999999999  11 digits  */
DEF INPUT PARAMETER  amount  AS DE.   /* MAX 9999999.99   7 + 2 dec */
DEF INPUT PARAMETER  acct-no AS c.    /* MAX 99999999     8 digits  */
DEF OUTPUT PARAMETER ocr     AS c.

DEF VAR ref       AS DE  NO-UNDO.
DEF VAR ref1      AS i NO-UNDO.
DEF VAR ref2      AS i  NO-UNDO.
DEF VAR cInvNum    AS c  NO-UNDO.
DEF VAR summa     AS c  NO-UNDO.
DEF VAR camount   AS c  NO-UNDO.



/* Calculate total LENGTH FOR 'kundreferens' : LENGTH of invoice no  + 2).
   Use MOD 10 IF LENGTH > 10 */

ref = (InvNum * 10) + ( (length(string(InvNum)) + 2) MODULO 10).


/* Calculate the LAST control digit */
RUN Mf/nnrela(INPUT ref, OUTPUT ref1).

/* Build the 'kundreferens' into CHAR VARIABLE */
cInvNum = string(ref) + string(ref1).

/* in Ticket Master Application the 'kundreferens' is MAX 11 digits,


   Invoice no.    MAX  9
   LENGTH digit        1
   Control digit       1           */


cInvNum = fill(" ",11 - length(cInvNum)) + cInvNum.

/* Calvulate the control digit FOR the amount */
RUN Mf/nnrela(INPUT amount * 100, OUTPUT ref2).

/* convert the amount into a CHAR VARIABLE */
camount = string(amount ,"-zzzzzzz.99").
/* Remove the DECIMAL point, substitute it BY a space */
substr(camount,length(camount) - 2,1) = " ".


/* Build the OCR STRING */

ASSIGN

ocr =
"                " + cInvNum + "  " + camount + "   " + string(ref2,"9").


/*
ocr =
"#                " + cInvNum + " #" + camount + "   " + string(ref2,"9") + " >"
+ fill(" ",24 - length(acct-no)) + acct-no + " #14#".
*/

/* Structure of the OCR STRING, positions numbered from right TO left -------

                inv. no.      amount                           AccNum no.
#...............999999999PV.#.9999999.99...9.>................99999999 #14#

7                         5 5          4   3 3                1       98  5
9                         3 1          0   6 4                7

....+....7....+....6....+....5....+....4....+....3....+....2....+....1....+....

*/

