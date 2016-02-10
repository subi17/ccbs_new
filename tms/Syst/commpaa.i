/* common alueen m‰‰ritys                                       commpaa.i    
*/

&IF "{&CommVarDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CommVarDef YES


DEF NEW SHARED VAR gcBrand       like customer.brand NO-UNDO.
DEF NEW SHARED VAR gcAllBrand    AS LOG NO-UNDO.

&IF "{&SKIP_FUNC_I}" NE "YES" 
&THEN
def new shared var ghFunc1 as handle.
if not valid-handle(ghFunc1) then run Func/func.p persistent set ghFunc1.
&ENDIF

/* yleinen j‰rj.nro */
def new shared var jno as int format "ZZZZZZZ9".
DEF NEW shared VAR jnotyyp AS INT.
def new shared var cfc as char format "x(24)".
def new shared var ctc as char format "x(24)".
def            var ccc as char format "x(24)".

/* otsikoissa n‰kyv‰t */
def new shared var ynimi   as char format "x(30)".
def new shared var pvm     as Date format "99-99-99" init TODAY.
def new shared var qcode   as c format "x(8)".
def new shared var qtitle  as c format "x(40)".
DEF NEW shared VAR qupd    AS lo.

def new shared var katun   as char format "x(8)".
def new shared var kanro   as int format "999".
def new shared var kanro2  as int format "999".
def new shared var karyhma as char format "x(10)".

/* ufkey.p:n tarvitsemat */
def new shared var toimi   as int  format "z".
def new shared var ehto    as int  format "z".
def new shared var sel_t   as char format "x(8)"  EXTENT 16.
def new shared var uft     as char format "x(16)" EXTENT 8.
def new shared var menu    as char format "x(16)" EXTENT 180.
def new shared var ufk     as int  format "z"     EXTENT 9.

/* formien otsikot */
def new shared var ots     as char format "x(78)".
DEF NEW shared VAR ots2    LIKE ots.

DEF NEW shared VAR nap     AS CHAR.    /* selauksissa KEYLABEL:in arvo */
def new shared var vast as char format "x".  /* messagen yhteydess‰ kysytt. */
def new shared var vali as int format "9".   /* ilmoittaa mit‰ valikoita */
DEF NEW shared VAR poisnap AS CHAR init
"tab,back-tab,f1,enter,return,cursor-up,cursor-down".

DEF NEW shared VAR TMSPrinter AS CHAR NO-UNDO.   /* kirjoittimen osoite */
DEF NEW shared VAR Modem AS CHAR NO-UNDO.
DEF NEW shared VAR soitto AS CHAR NO-UNDO. /* ohjauskomento modemille */
DEF NEW shared VAR yvari AS LOG NO-UNDO.
DEF NEW shared VAR ymoni AS LOG NO-UNDO.
DEF NEW shared VAR ykirje AS INT NO-UNDO.

def new shared var gcHelpParam as char NO-UNDO.
def new shared var si-hlo      as char format "x(50)".
def new shared var si-nro as char format "x(16)".
DEF NEW shared VAR si-pvm AS Date NO-UNDO.
DEF NEW shared VAR si-recid AS RECID NO-UNDO.
DEF NEW shared VAR si-recid2 AS RECID NO-UNDO.
def new shared var si-tunnus as char format "x(6)".
def new shared var si-ryno   as char format "x(16)".
DEF NEW shared VAR multiuser AS LOG NO-UNDO.
DEF NEW shared VAR helpkey AS CHAR NO-UNDO.

DEF NEW SHARED VAR rt_param AS C format "x(50)" EXTENT 20 NO-UNDO.

DEF NEW SHARED VAR ergo-kbd AS LO NO-UNDO.


DEF VAR Passwd AS c NO-UNDO.

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

