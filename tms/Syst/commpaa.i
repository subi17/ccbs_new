/* common alueen m‰‰ritys                                       commpaa.i    
*/

&IF "{&CommVarDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CommVarDef YES

/* otsikoissa n‰kyv‰t */
def new shared var katun   as char format "x(8)".

/* ufkey.p:n tarvitsemat */

on f1 go.
on f2 BELL.
on f4 BELL.

on f9 HELP.

on f12 anywhere DO:
   message "EventLog function is NOT active here"
   VIEW-AS ALERT-BOX information.
END.

on f21 HELP.
status INPUT off.

&ENDIF

