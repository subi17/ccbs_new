/* --------------------------------------------------------------------
  MODULE .......: creaters.p
  FUNCTION .....: Create salesmen for reseller
  APPLICATION ..: TMS
  AUTHOR .......: tk  
  CREATED ......: 17.06.04 
  MODIFIED .....: 10.02.05 tk lcGroups
                  14.02.05 tk lcRoles
  
  VERSION ......: TeleF
  ------------------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/email.i}

DEF VAR lcResell LIKE Reseller.Reseller NO-UNDO.
DEF VAR lcRSName LIKE Reseller.RSName   NO-UNDO.
DEF VAR liCount  AS I FORMAT "zz9"      NO-UNDO INIT 1.
DEF VAR llWeb    AS LOG                 NO-UNDO.
DEF VAR lcEmail  LIKE TMSUser.Email     NO-UNDO.
DEF VAR loop     AS I                   NO-UNDO.
DEF VAR llOldRS  AS LOG                 NO-UNDO.
DEF VAR liFirst  AS I                   NO-UNDO.
DEF VAR lcGroups AS CH                  NO-UNDO.
DEF VAR lcRoles  AS CH                  NO-UNDO.
DEF VAR loop2    AS I                   NO-UNDO.

form
 skip(2)
 "      This program will create reseller and given number of salesmen"
 "      Passwords will be sent via email to given address."
 skip(2)
 "         Reseller Code ......:" lcResell SKIP
 "         Reseller Name ......:" lcRSName SKIP
 "         Number of salesmen .:" liCount  SKIP
 "         User Groups ........:" lcGroups format "x(40)" SKIP
 "         Web passwords ......:" llWeb    SKIP
 "         Profiles ...........:" lcRoles  format "x(40)" SKIP
 "         Email address ......:" lcEmail  
 skip(4)
with row 1 width 80 NO-LABELS
   title " " + ynimi + " CREATE RESELLER " + string(pvm,"99-99-99") + " "
FRAME rajat.


FIND FIRST TMSUser NO-LOCK WHERE
           TMSUser.UserCode = katun NO-ERROR.
IF AVAIL TMSUser THEN lcEMail = TMSUser.Email.
ELSE lcEMail = "".

loop:
repeat with frame rajat:
   PAUSE 0 no-message.
   ehto = 9. RUN Syst/ufkey.
   UPDATE 
      lcResell
      lcRSName
      liCount 
      lcGroups
      llWeb
      lcRoles
      lcEMail
      
   EDITING:
      readkey.
      
      HIDE MESSAGE NO-PAUSE.
      
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         IF FRAME-FIELD = "lcResell" THEN DO:
            ASSIGN lcResell.
            IF lcResell = "" THEN RETURN.
            
            FIND FIRST Reseller NO-LOCK WHERE
                       Reseller.Brand  = gcBrand AND
                       Reseller.Reseller = lcResell NO-ERROR.
            IF AVAIL Reseller THEN DO:
               MESSAGE 
                  "Reseller " + lcResell + " already exists !" SKIP
                  "Create salesmen to this reseller ?"
               VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE llOldRS.
               IF NOT llOldRS THEN NEXT.
               ELSE DISP Reseller.RSName @ lcRSName.
            END.
            ELSE llOldRS = FALSE.
         END.
      
      END.

      apply lastkey.
   END.
   
   ASSIGN
      ufk = 0
      ufk[1] = 132
      ufk[5] = 795
      ufk[8] = 8
      ehto = 0.

   RUN Syst/ufkey.
   case toimi:
      when 8 then return.
      when 1 then next loop.
      when 5 then leave loop.
   end.

end.

if keylabel(lastkey) = "f4" then return.

IF NOT llOldRS THEN DO:
   CREATE Reseller.
   ASSIGN
      Reseller.Brand    = gcBrand 
      Reseller.Reseller = lcResell 
      Reseller.RsName   = lcRSName
      liFirst = 1.

END.   
ELSE DO:
   FIND LAST Salesman USE-INDEX Salesman NO-LOCK WHERE
             Salesman.Reseller = lcResell AND
             Salesman.Salesman BEGINS lcResell
   NO-ERROR.

   IF NOT AVAIL Salesman THEN liFirst = 1.
   ELSE DO:
      ASSIGN
         liFirst = INT(SUBSTR(Salesman.salesman,length(lcResell) + 1)) + 1
      NO-ERROR.
      IF ERROR-STATUS:ERROR THEN liFirst = 1.
   END.
END.

output stream excel to /tmp/smpasswords.txt.

DO loop = liFirst to liFirst + liCount - 1:

   CREATE salesman.
   ASSIGN
      SalesMan.Brand    = gcBrand
      SalesMan.Salesman = lcResell + string(loop,"999")
      SalesMan.Reseller = lcResell
      SalesMan.SmName   = Reseller.RsName
      SalesMan.RsLevel  = 0.


   IF llWeb THEN DO:
              
      FIND FIRST username EXCLUSIVE-LOCK WHERE
                 username.usercode = SalesMan.Salesman NO-ERROR.
      IF NOT AVAIL username THEN DO:
         CREATE username.
         ASSIGN 
            username.usercode   = SalesMan.Salesman
            username.password   = string(random(10000,99999))
            username.usergroups = lcGroups.
      END.

      ASSIGN 
         username.lastname     = Salesman.SMName
         username.menulanguage = "fin".

      PUT STREAM excel UNFORMATTED
         Username.Usercode tab
         username.password skip.
         
      DO loop2 = 1 TO NUM-ENTRIES(lcRoles):
         create userkeys.
         assign
            userkeys.usercode = username.usercode
            userkeys.keytype  = entry(loop2,lcRoles)
            userkeys.key      = "1".
         if userkeys.keytype = "SALESMAN" then 
            userkeys.key = userkeys.usercode.   
      END.
   END.

END.   
        
output stream excel close.

IF llWeb THEN
SendAttMail("'WEB Passwords'",lcEMail,"/tmp/smpasswords.txt").

MESSAGE "All done !" VIEW-AS ALERT-BOX.
        
