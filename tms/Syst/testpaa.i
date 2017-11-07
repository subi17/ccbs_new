/* common alueen m{{ritys                                       commpaa.i    */

&IF "{&CommVarDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CommVarDef YES

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

DEFINE VARIABLE gcTempBrand AS CHARACTER NO-UNDO.

ASSIGN
   Syst.Var:katun = "admin"
   Syst.Var:yvari = TRUE
   Syst.Var:ynimi = "!!! TESTI !!!"
   Syst.Var:gcAllBrand = TRUE
   Syst.Var:gcBrand    = "1"
   gcTempBrand = Syst.Var:gcBrand
   Syst.Var:qupd = TRUE.

update gcTempBrand label "Brand"
with side-labels row 10 centered title " Default brand " frame tstfram.

hide frame tstfram.

Syst.Var:gcBrand = gcTempBrand.

/* set propath etc. */
RUN Syst/testbr.p.

&ENDIF