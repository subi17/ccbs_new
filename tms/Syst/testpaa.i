/* common alueen m{{ritys                                       commpaa.i    */

&IF "{&CommVarDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CommVarDef YES

/* otsikoissa näkyvät */

def new shared var katun   as char format "x(8)".

/* ufkey.p:n tarvitsemat */
def new shared var ehto    as int  format "z".
def new shared var sel_t   as char format "x(8)"  EXTENT 16.
def new shared var ufk     as int  format "z"     EXTENT 9.

DEF NEW shared VAR nap     AS CHAR.    /* selauksissa KEYLABEL:in arvo */
DEF NEW shared VAR poisnap AS CHAR init
"tab,back-tab,f1,enter,return,cursor-up,cursor-down". 

DEF NEW shared VAR TMSPrinter AS CHAR NO-UNDO.   /* kirjoittimen osoite */
DEF NEW shared VAR yvari AS LOG NO-UNDO.

def new shared var gcHelpParam as char NO-UNDO.
DEF NEW shared VAR si-recid AS RECID NO-UNDO.
DEF NEW shared VAR si-recid2 AS RECID NO-UNDO.
DEF NEW shared VAR helpkey AS CHAR NO-UNDO.
DEF NEW shared VAR ginvno  AS i NO-UNDO.


DEF NEW SHARED VAR ergo-kbd AS LO NO-UNDO.

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


katun = "admin".  yvari = true. Syst.CUICommon:ynimi = "!!! TESTI !!!".
Syst.CUICommon:gcAllBrand = TRUE.
Syst.CUICommon:gcBrand    = "1".

DEFINE VARIABLE gcTempBrand AS CHARACTER NO-UNDO.
gcTempBrand = Syst.CUICommon:gcBrand.
update gcTempBrand label "Brand"
with side-labels row 10 centered title " Default brand " frame tstfram.

hide frame tstfram.

Syst.CUICommon:gcBrand = gcTempBrand.
Syst.CUICommon:qupd = TRUE.

/* set propath etc. */
RUN Syst/testbr.p.

&ENDIF

