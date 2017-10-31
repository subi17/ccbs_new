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
   Syst.CUICommon:katun = "admin"
   Syst.CUICommon:yvari = TRUE
   Syst.CUICommon:ynimi = "!!! TESTI !!!"
   Syst.CUICommon:gcAllBrand = TRUE
   Syst.CUICommon:gcBrand    = "1"
   gcTempBrand = Syst.CUICommon:gcBrand
   Syst.CUICommon:qupd = TRUE.

update gcTempBrand label "Brand"
with side-labels row 10 centered title " Default brand " frame tstfram.

hide frame tstfram.

Syst.CUICommon:gcBrand = gcTempBrand.

/* set propath etc. */
RUN Syst/testbr.p.

&ENDIF