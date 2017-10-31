/* common alueen m{{ritys                                        commali.i    
            07.10.03/aam CommVarDef
            09.02.04/aam gcHelpParam
*/

&IF "{&CommVarDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CommVarDef YES

def new shared var pvm     as Date format "99-99-99".
def new shared var qupd    AS lo.

def new shared var katun   as char format "x(8)".

def new shared var toimi   as int  format "z".
def new shared var ehto    as int  format "z".
def new shared var sel_t   as char format "x(8)"  EXTENT 16.
def new shared var ufk     as int  format "z"     EXTENT 9.

def new shared var nap     AS CHAR.
def new shared var poisnap AS CHAR.

def new shared var TMSPrinter AS CHAR NO-UNDO.   /* kirjoittimen osoite */
def new shared var yvari AS LOG NO-UNDO.

def new shared var si-pvm      AS Date NO-UNDO.
def new shared var si-recid    AS RECID NO-UNDO.
def new shared var si-recid2   AS RECID NO-UNDO.
def new shared var gcHelpParam as char NO-UNDO.

def new shared var helpkey AS CHAR NO-UNDO.

&GLOBAL-DEFINE BrandVarDefined YES

def new shared var ergo-kbd AS LO NO-UNDO.

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

Syst.CUICommon:gcBrand = "1".

&ENDIF


