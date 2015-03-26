/* common alueen m{{ritys                                        commali.i    
            07.10.03/aam CommVarDef
            09.02.04/aam gcHelpParam
*/

&IF "{&CommVarDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CommVarDef YES


&IF "{&SKIP_FUNC_I}" NE "YES" 
&THEN
   def shared var ghFunc1 as handle.
   if not valid-handle(ghFunc1) then run func.i persistent set ghFunc1.
&ENDIF


def shared var jno as int format "ZZZZZZZ9".
DEF NEW shared VAR jnotyyp AS INT.
def shared var cfc as char format "x(24)".
def shared var ctc as char format "x(24)".
def        var ccc as char format "x(24)".

def shared var ynimi   as char format "x(30)".
def shared var pvm     as Date format "99-99-99".
def shared var qcode   as c format "x(8)".
def shared var qtitle  as c format "x(40)".
DEF shared VAR qupd    AS lo.


/* runtime parameters */
DEF SHARED VAR rt_param AS C format "x(50)" EXTENT 20 NO-UNDO.


def shared var katun   as char format "x(8)".
def shared var kanro   as int  format "999".
def shared var kanro2  as int  format "999".
def shared var karyhma as char format "x(10)".

def shared var toimi   as int  format "z".
def shared var ehto    as int  format "z".
def shared var sel_t   as char format "x(8)"  EXTENT 16.
def shared var uft     as char format "x(16)" EXTENT 8.
def shared var menu    as char format "x(16)" EXTENT 180.
def shared var ufk     as int  format "z"     EXTENT 9.

def shared var ots     as char format "x(78)".
DEF shared VAR ots2    LIKE ots.

DEF shared VAR nap     AS CHAR.
def shared var vast as char format "x".
def shared var vali    as int format "9".
DEF shared VAR poisnap AS CHAR.

DEF shared VAR TMSPrinter AS CHAR NO-UNDO.   /* kirjoittimen osoite */
DEF shared VAR Modem AS CHAR NO-UNDO.
DEF shared VAR soitto AS CHAR NO-UNDO. /* ohjauskomento modemille */
DEF shared VAR yvari AS LOG NO-UNDO.
DEF shared VAR ymoni AS LOG NO-UNDO.
DEF shared VAR ykirje AS INT NO-UNDO.

DEF shared VAR si-pvm      AS Date NO-UNDO.
DEF shared VAR si-recid    AS RECID NO-UNDO.
DEF shared VAR si-recid2   AS RECID NO-UNDO.
def shared var gcHelpParam as char NO-UNDO.
DEF shared VAR pankki AS LOG NO-UNDO.
DEF shared VAR vava AS LOG NO-UNDO.
DEF VAR Passwd AS c NO-UNDO.

DEF shared VAR multiuser AS LOG NO-UNDO.
DEF shared VAR helpkey AS CHAR NO-UNDO.

&GLOBAL-DEFINE BrandVarDefined YES

DEF SHARED VAR gcBrand       like customer.brand NO-UNDO.
DEF SHARED VAR gcAllBrand    AS LOG    NO-UNDO.

DEF SHARED VAR ergo-kbd AS LO NO-UNDO.

DEF VAR ldEurKerr AS DE init 5.94573.

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


