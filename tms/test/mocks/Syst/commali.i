/* common alueen m{{ritys                                        commali.i    
            07.10.03/aam CommVarDef
            09.02.04/aam Syst.Var:gcHelpParam
*/

&IF "{&CommVarDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CommVarDef YES

on f1 go.
on f2 go.
on f4 go.
on f9 HELP.

on f12 anywhere DO:
   message "EventLog function is NOT active here"
   VIEW-AS ALERT-BOX information.
END.


on f21 HELP.
status INPUT off.

&ENDIF