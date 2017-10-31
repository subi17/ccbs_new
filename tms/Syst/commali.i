/* common alueen m{{ritys                                        commali.i    
            07.10.03/aam CommVarDef
            09.02.04/aam gcHelpParam
*/

&IF "{&CommVarDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CommVarDef YES


def shared var ynimi   as char format "x(30)".
def shared var pvm     as Date format "99-99-99".
DEF shared VAR qupd    AS lo.

def shared var katun   as char format "x(8)".

def shared var toimi   as int  format "z".
def shared var ehto    as int  format "z".
def shared var sel_t   as char format "x(8)"  EXTENT 16.
def shared var ufk     as int  format "z"     EXTENT 9.

DEF shared VAR nap     AS CHAR.
DEF shared VAR poisnap AS CHAR.

DEF shared VAR TMSPrinter AS CHAR NO-UNDO.   /* kirjoittimen osoite */
DEF shared VAR yvari AS LOG NO-UNDO.

DEF shared VAR si-pvm      AS Date NO-UNDO.
DEF shared VAR si-recid    AS RECID NO-UNDO.
DEF shared VAR si-recid2   AS RECID NO-UNDO.
def shared var gcHelpParam as char NO-UNDO.

DEF shared VAR helpkey AS CHAR NO-UNDO.

&GLOBAL-DEFINE BrandVarDefined YES

DEF SHARED VAR gcBrand       like customer.brand NO-UNDO.
DEF SHARED VAR gcAllBrand    AS LOG    NO-UNDO.

DEF SHARED VAR ergo-kbd AS LO NO-UNDO.

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


