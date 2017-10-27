/* common alueen m��ritys                                       commpaa.i    
*/

&IF "{&CommVarDef}" NE "YES" 
&THEN

&GLOBAL-DEFINE CommVarDef YES


DEF NEW SHARED VAR gcBrand       like customer.brand NO-UNDO.
DEF NEW SHARED VAR gcAllBrand    AS LOG NO-UNDO.

&IF "{&SKIP_FUNC_I}" NE "YES" 
&THEN
def new shared var ghFunc1 as handle.
if not valid-handle(ghFunc1) then RUN Func/func.p persistent set ghFunc1.
&ENDIF

def new shared var cfc as char format "x(24)".
def new shared var ctc as char format "x(24)".
def            var ccc as char format "x(24)".

/* otsikoissa n�kyv�t */
def new shared var ynimi   as char format "x(30)".
def new shared var pvm     as Date format "99-99-99" init TODAY.
DEF NEW shared VAR qupd    AS lo.

def new shared var katun   as char format "x(8)".

/* ufkey.p:n tarvitsemat */
def new shared var toimi   as int  format "z".
def new shared var ehto    as int  format "z".
def new shared var sel_t   as char format "x(8)"  EXTENT 16.
def new shared var ufk     as int  format "z"     EXTENT 9.

DEF NEW shared VAR nap     AS CHAR.    /* selauksissa KEYLABEL:in arvo */
DEF NEW shared VAR poisnap AS CHAR init
"tab,back-tab,f1,enter,return,cursor-up,cursor-down".

DEF NEW shared VAR TMSPrinter AS CHAR NO-UNDO.   /* kirjoittimen osoite */
DEF NEW shared VAR yvari AS LOG NO-UNDO.

def new shared var gcHelpParam as char NO-UNDO.
DEF NEW shared VAR si-pvm AS Date NO-UNDO.
DEF NEW shared VAR si-recid AS RECID NO-UNDO.
DEF NEW shared VAR si-recid2 AS RECID NO-UNDO.
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

