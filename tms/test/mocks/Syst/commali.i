/* common alueen m{{ritys                                        commali.i    
            07.10.03/aam CommVarDef
            09.02.04/aam gcHelpParam
*/

&IF "{&CommVarDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CommVarDef YES


&IF "{&SKIP_FUNC_I}" NE "YES" 
&THEN
   def new shared var ghFunc1 as handle.
   if not valid-handle(ghFunc1) then run Func/func.p persistent set ghFunc1.
&ENDIF


def new shared var jno as int format "ZZZZZZZ9".
DEF NEW shared VAR jnotyyp AS INT.
def new shared var cfc as char format "x(24)".
def new shared var ctc as char format "x(24)".
def        var ccc as char format "x(24)".

def new shared var ynimi   as char format "x(30)".
def new shared var pvm     as Date format "99-99-99".
def new shared var qcode   as c format "x(8)".
def new shared var qtitle  as c format "x(40)".
def new shared var qupd    AS lo.


/* runtime parameters */
def new shared var rt_param AS C format "x(50)" EXTENT 20 NO-UNDO.


def new shared var katun   as char format "x(8)".
def new shared var kanro   as int  format "999".
def new shared var kanro2  as int  format "999".
def new shared var karyhma as char format "x(10)".

def new shared var toimi   as int  format "z".
def new shared var ehto    as int  format "z".
def new shared var sel_t   as char format "x(8)"  EXTENT 16.
def new shared var uft     as char format "x(16)" EXTENT 8.
def new shared var menu    as char format "x(16)" EXTENT 180.
def new shared var ufk     as int  format "z"     EXTENT 9.

def new shared var ots     as char format "x(78)".
def new shared var ots2    LIKE ots.

def new shared var nap     AS CHAR.
def new shared var vast as char format "x".
def new shared var vali    as int format "9".
def new shared var poisnap AS CHAR.

def new shared var TMSPrinter AS CHAR NO-UNDO.   /* kirjoittimen osoite */
def new shared var Modem AS CHAR NO-UNDO.
def new shared var soitto AS CHAR NO-UNDO. /* ohjauskomento modemille */
def new shared var yvari AS LOG NO-UNDO.
def new shared var ymoni AS LOG NO-UNDO.
def new shared var ykirje AS INT NO-UNDO.

def new shared var si-pvm      AS Date NO-UNDO.
def new shared var si-recid    AS RECID NO-UNDO.
def new shared var si-recid2   AS RECID NO-UNDO.
def new shared var gcHelpParam as char NO-UNDO.
def new shared var pankki AS LOG NO-UNDO.
def new shared var vava AS LOG NO-UNDO.
DEF VAR Passwd AS c NO-UNDO.

def new shared var multiuser AS LOG NO-UNDO.
def new shared var helpkey AS CHAR NO-UNDO.

&GLOBAL-DEFINE BrandVarDefined YES

def new shared var gcBrand       like customer.brand NO-UNDO.
def new shared var gcAllBrand    AS LOG    NO-UNDO.

def new shared var ergo-kbd AS LO NO-UNDO.

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


