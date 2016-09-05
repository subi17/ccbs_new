/*-------------------------------------------------------------------------
  MODULE .......: UKAYT.P
  KUTSUVAMODULI :
  FUNCTION .....: KAyttAjien yllApito
  SOVELLUTUS ...: TS
  AUTHOR .......: TT
  CREATED ......: 10.06.1991
  MODIFIED .....: 03.04.92 /tt
                  25.09.96 /tt --> Ruotsiksi, poistettu ylim. kentat ja valikot
                  30.10.97 /pt --> RepDir FIELD
                  25.06.98 /kl --> vast => ok
                  09.02.99 /pt --> memberships in user groups, f4
                  28.04.99 /pt --> unnecessary fields hided, F4 moved -> F3,
                                   user rights into F4
                  25.04.02 lp added EMail
                  20.08.02 jp brandcode
                  06.11.02 jr Eventlog 
                  21.02.03 tk UserGroup added, UserRight removed
                  06.03.03 tk tokens
                  18.03.03 tk fixed ufkey problems in add/change
                  25.03.03 tk show groupcode in frame sel
                              show "SECRET" @ password 
                  26.03.03/aam CreditLimit            
                  22.03.04/aam Salesmen (usersman)
                  28.10.05/mvi foreignid, fromdate & todate added.
                  31.10.05/mvi repdir, email defaults set for telef
                  28.12.05/mvi haku nimellä, selaus nimellä, siistimistä
                  31.05.06/aam get RepDir from cparam
                  21.07.06 kl  ctrl-p
                  12.01.07/mvi Change passwords with admin tool (CTRL-P)
                  02.06.07/mvi Yoigo version, disabled CTRL-P usage 
                               moved feature to admin menu

  Version ......: Yoigo
  ----------------------------------------------------------------------- */

{Func/timestamp.i} 
{Func/chkmail.i}
{Syst/commali.i} 
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'TMSUser'}
{Func/cparam2.i}
{Func/fuserright.i}


DEF NEW shared VAR siirto AS CHAR.

DEF VAR ok AS lo NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR order AS INT NO-UNDO.              
DEF VAR ex-order AS INT NO-UNDO.

DEF VAR firstline AS INT NO-UNDO.
DEF VAR rtab AS RECID EXTENT 15 NO-UNDO.
DEF VAR ufkey AS LOG NO-UNDO.
DEF VAR must-add AS LOG NO-UNDO.
DEF VAR must-print AS LOG NO-UNDO.
DEF VAR fr-header    AS CHAR.

DEF VAR memory   AS RECID.
def var line     as int format "99".
DEF VAR nro      LIKE TMSUser.UserNum.
DEF VAR ha-nro   LIKE TMSUser.UserNum.
DEF VAR ha-nimi  LIKE TMSUser.UserCode.
DEF VAR moremail     AS CHAR   init ""     NO-UNDO.
DEF VAR ret          AS i                      NO-UNDO.

DEF VAR lcAdmin      AS CHAR               NO-UNDO.
DEF VAR lcUserCode   AS CHAR               NO-UNDO.
DEF VAR lcOldPWD     AS CHAR               NO-UNDO.
DEF VAR lcNewPWD1    AS CHAR               NO-UNDO.
DEF VAR lcNewPWD2    AS CHAR               NO-UNDO.
DEF VAR llAdminUser  AS LOG                NO-UNDO.
DEF VAR lcExclGroup  AS CHAR               NO-UNDO INIT "NOTinUSE".



IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTMSUser AS HANDLE NO-UNDO.
   lhTMSUser = BUFFER TMSUser:HANDLE.
   RUN StarEventInitialize(lhTMSUser).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhTMSUser).
   END.
END.


form /* pAAruutu, scroll */
    TMSUser.UserCode  column-label "User ID"
    help "User's individual ID"
    TMSUser.UserName column-label "Username"   format "x(17)"
    help "User's Name"
    TMSUser.Initials  column-label "Init"
    TMSUser.foreignid FORMAT "x(10)" COLUMN-LABEL "ForeignID"
    TMSUser.usergroup FORMAT "x(5)" column-label "Group"
    TMSUSer.fromdate FORMAT "99.99.9999" column-LABEL "From"
    TMSUser.toDate   FORMAT "99.99.9999" column-LABEL "Until"
WITH
    width 80 OVERLAY scroll 1 15 DOWN COLOR value(cfc)
    title color value(ctc) " " + ynimi + " USER IDS "
    + string(pvm,"99-99-99") + " " FRAME sel.

form /* lisAyksiA varten, ei scroll */
    
    TMSUser.foreignid LABEL "Person id.............."
       HELP "Unique ID for user"  SKIP
    TMSUser.UserCode    label   "User id ..............."
       HELP "Unique ID for an user"    
    TMSUser.UserName format "x(40)" label "User's name ..........."
       HELP "User's name" SKIP
    TMSUser.Initials    label "User's initials ......."
       HELP "Initials"  SKIP
    TMSUser.Password         label "Password .............."
       HELP "User's password"  SKIP
    TMSUser.UserGroup   label "UserGroup ............." 
        validate(tmsuser.usergroup NE "" AND 
                 can-find(first usergrp where 
                                 usergrp.usergroup = input tmsuser.usergroup),
                 "unknown group") SKIP
    TMSUser.AdminUser LABEL "Admin User ............" SKIP            
    TMSUSer.fromdate LABEL "Valid From............." FORMAT "99.99.9999"
       HELP "Account valid from this date"
    TMSUser.todate LABEL "Valid Until" FORMAT "99.99.9999" 
       HELP "Account valid until this date" SKIP(1)
    RepDir              label "Dir for .txt -printouts"
       HELP "Directory for .txt printouts"
    TMSUser.Brand       label "Allowed brands ........"    
       HELP "* = all brands (Brand1,Brand2,...)"              SKIP
    TMSUser.StartMenu     label "Start Menu ............" format "x(10)"
       HELP "Default Function Code (start menu)" SKIP  
/*    TMSUser.CreditLimit LABEL "Crediting Limit ......."  HELP "User's credit limit" SKIP */
    TMSUser.Email     LABEL "User's e-mail address ." format "x(50)"   
       HELP "User's e-mail address"            SKIP
    moremail       NO-LABEL format "x(75)"
       HELP "More E-mail addresses" 
WITH  OVERLAY ROW 2 centered COLOR value(cfc)
    TITLE COLOR value(ctc) fr-header WITH side-labels
        FRAME lis.

form /* KAyttAjAn hakua varten */
    ha-nimi FORMAT "x(20)"
    help "Enter User TMS ID"    
    WITH ROW 4 col 2
    title color value(ctc) " SEEK USER TMS ID "     
    NO-LABELS COLOR value(cfc) OVERLAY FRAME syha1.

form /* numeron hakua varten */
    ha-nimi format "x(20)"
    help "Enter Foreign ID"      
    with row 4 col 2 title color value(ctc) " SEEK BY FOREIGN ID "
    NO-LABELS COLOR value(cfc) OVERLAY FRAME syha2.

form /* Nimen hakua varten */
    ha-nimi FORMAT "x(30)"
    help "Enter User Name"    
    WITH ROW 4 col 2
    title color value(ctc) " SEEK USER BY NAME "     
    NO-LABELS COLOR value(cfc) OVERLAY FRAME syha3.

FORM
   "UserID :" lcUserCode
   "Old PWD:" lcOldPWD
   "New PWD:" lcNewPWD1
   "Repeat :" lcNewPWD2
WITH NO-LABELS CENTERED ROW 4
FRAME PWD.

llAdminUser = fIsAdminUser(katun).

 cfc = "sel". RUN Syst/ufcolor.p. ccc = cfc.
 view FRAME sel.

order = 1.
RUN local-find-first.

 IF AVAILABLE TMSUser THEN DO:
    ASSIGN
    must-add = FALSE
    must-print = TRUE
    memory = recid(TMSUser).
 END.
 ELSE DO:
    IF lcRight NE "RW" THEN DO:
       MESSAGE "No TMS users available !" VIEW-AS ALERT-BOX.
       RETURN.
    END.
    ASSIGN
    must-add = TRUE
    must-print = FALSE
    fr-header = " ADD ".

 END.

 ASSIGN
 ufkey = TRUE
 firstline = 0.

BROWSE:
    repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

       IF order <> ex-order THEN DO:
          ex-order = order.
       END.

       ufkey = TRUE.

       IF must-add THEN DO:
          cfc = "lis". RUN Syst/ufcolor.p.
          ehto = 9. RUN Syst/ufkey.p. ufkey = true.
          PAUSE 0 no-message.
add-new:
          repeat WITH FRAME lis:
             PAUSE 0. 
             CLEAR FRAME lis no-pause.

             /* mAAritellAAn uuden kAyttAjAn numero */
             FIND LAST TMSUser USE-INDEX UserNum no-lock no-error.
             IF AVAILABLE TMSUser THEN nro = TMSUser.UserNum + 1.
             ELSE nro = 1.
             PROMPT-FOR TMSUser.UserCode 
                validate(input UserCode = "" OR 
             NOT can-find(TMSUser where TMSUser.UserCode = INPUT UserCode),
             "User with this ID exists already !").


             if input TMSUser.UserCode = "" THEN UNDO,leave add-new.

             CREATE TMSUser.
             ASSIGN
             memory          = recid(TMSUser)
             TMSUser.UserCode
             TMSUser.UserNum = nro
             TMSUser.email   = "crm@yoigo.com"
             TMSUser.brand   = "1"
             TMSUser.AdminUser   = FALSE
             TMSUSER.TopUpLimit  = 0
             TMSUser.fromdate    = TODAY
             TMSUser.UserGroup   = "Syst".
             TMSUser.RepDir      = fCParamC("PrintDir").
             PAUSE 0 no-message.
            
             RUN pUpdateUser.
                
            /* create TMSPass entry now */
            CREATE tmspass.
            ASSIGN
               tmspass.createts = fmakets()
               tmspass.usercode = tmsuser.usercode
               tmspass.creator = katun
               tmspass.password = tmsuser.password.
      
     addEMail: REPEAT WITH FRAME lis:
             IF moremail NE "" THEN
                ASSIGN
                TMSUser.Email = SUBSTRING(TMSUser.Email,1,50) + moremail
                moremail      = "".
             fIsEmail(TMSUser.Email, OUTPUT ret).

             IF ret NE 1 AND TMSUser.Emai NE "" THEN DO:
                BELL.
                MESSAGE "Invalid E-mail address line!".
                UPDATE TMSUser.Email moremail.
                NEXT addemail.
             END.  
             ELSE LEAVE addemail.
     END. /* addemail */
       

             HIDE FRAME lis no-pause.
          END.  /* add-new */

          HIDE FRAME lis no-pause.
          IF NOT can-find(FIRST TMSUser) THEN RETURN.
          must-print = TRUE.
          must-add = FALSE.
          cfc = "sel". RUN Syst/ufcolor.p.

          IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTMSUser).

       END. /* lisAttAvA */

       IF must-print THEN DO:
          FIND TMSUser where recid(TMSUser) = memory no-lock.
          /*
          up FRAME-LINE - 1.
          */
          rtab = ?.
          repeat WITH FRAME sel:
             IF AVAILABLE TMSUser THEN DO:
                DISPLAY 
                   TMSUser.UserCode 
                   TMSUser.UserName 
                   TMSUser.Initials 
                   TMSUser.Usergroup
                   TMSUser.foreignid
                   TMSUser.fromdate
                   TMSUser.todate.
                rtab[FRAME-LINE] = recid(TMSUser).
                RUN local-find-next.           
             END.
             ELSE CLEAR FRAME sel no-pause.
             IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
             DOWN 1.
          END.  /* repeat */
          up FRAME-LINE - 1.
          DOWN firstline.
          firstline = 0.
          must-print = FALSE.
       END. /* must-print */

       IF ufkey THEN DO:
          ASSIGN
          ufk = 0
          ufk[1] = 9038  ufk[2] = 9039   ufk[3] = 30 
          ufk[4] = (IF lcExclGroup = "" THEN 46 /* show only actives */ ELSE  /* show all users */ 47)
          ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
          ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
          ufk[7] = 0   ufk[8] = 8  ufk[9] = 1
          ufkey = FALSE ehto = 3.
          RUN Syst/ufkey.p.
       END.

       PAUSE 0 no-message.

       IF order = 1 THEN DO:
          CHOOSE ROW TMSUser.UserCode {Syst/uchoose.i} no-error WITH FRAME sel.
          COLOR DISPLAY value(ccc) TMSUser.UserCode WITH FRAME sel.
       END.
       IF order = 2 THEN DO:
          CHOOSE ROW TMSUser.UserName {Syst/uchoose.i}
             no-error WITH FRAME sel.
          COLOR DISPLAY value(ccc) TMSUser.UserName WITH FRAME sel.
       END.
       IF order = 3 THEN DO:
          CHOOSE ROW TMSUser.ForeignID {Syst/uchoose.i}
             no-error WITH FRAME sel.
          COLOR DISPLAY value(ccc) TMSUser.ForeignID WITH FRAME sel.
       END.


       IF rtab[FRAME-LINE] = ? THEN NEXT.

       nap = keylabel(LASTKEY).

       if nap = "cursor-left" THEN DO:
          order = order - 1.
          IF order = 0 THEN order = 3.
       END.

       if nap = "cursor-right" THEN DO:
          order = order + 1.
          IF order = 4 THEN order = 1.
       END.

       IF order <> ex-order THEN DO:
          ASSIGN
          firstline = 0
          memory = rtab[FRAME-LINE].
          FIND TMSUser where recid(TMSUser) = memory.
          DO i = 1 TO FRAME-LINE - 1:
             RUN local-find-prev.
             IF AVAILABLE TMSUser THEN DO:
                ASSIGN
                firstline = i
                memory = recid(TMSUser).
             END.
             ELSE LEAVE.
          END.
          must-print = TRUE.
          NEXT BROWSE.
       END.

       /* haku */
       if nap = "f1"  or nap = "1" THEN DO:  /* tms tunnuksen haku */
          cfc = "haku". RUN Syst/ufcolor.p.
          ha-nimi = "".
          UPDATE ha-nimi WITH FRAME syha1.
          HIDE FRAME syha1.
          if ha-nimi <> "" THEN DO:
             order = 1.
             FIND FIRST TMSUser where TMSUser.UserCode = ha-nimi and LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
                no-lock no-error.
             IF NOT AVAILABLE TMSUser THEN DO:
                FIND FIRST TMSUser where TMSUser.UserCode ge ha-nimi  and LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0
                no-lock no-error.
                IF NOT AVAILABLE TMSUser THEN DO:
                   message "NOT FOUND".
                   cfc = "sel". RUN Syst/ufcolor.p.
                   NEXT BROWSE.
                END.
             END.
             /*  kayttnro was found */
             ASSIGN
             memory = recid(TMSUser)
             must-print = TRUE
             order = 1.
             cfc = "sel". RUN Syst/ufcolor.p.
             NEXT BROWSE.
          END. /* tunnuksen haku */
       END. /* f1 */

       else if nap = "f2" or nap = "2" THEN DO: /* foreign id haku */
          cfc = "haku". RUN Syst/ufcolor.p.
          ha-nimi = "".
          UPDATE ha-nimi WITH FRAME syha2.
          HIDE FRAME syha2.
          IF ha-nimi <> "" THEN DO:
             /* haetaan foreign id nroa */

             FIND FIRST TMSUser where TMSUser.ForeignID = ha-nimi and LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
             USE-INDEX ForeignID no-lock no-error.
             IF NOT AVAILABLE TMSUser THEN DO:
                FIND FIRST TMSUser where TMSUser.ForeignID > ha-nimi and LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
                USE-INDEX ForeignId no-lock no-error.
                IF NOT AVAILABLE TMSUser THEN DO:
                   message "NOT FOUND".
                   cfc = "sel". RUN Syst/ufcolor.p.
                   NEXT BROWSE.
                END.
             END.
             /*  foreignid was found */
             memory = recid(TMSUser).
             must-print = TRUE.
             order = 3.
             cfc = "sel". RUN Syst/ufcolor.p.
             NEXT BROWSE.
          END. /* haku foreignidllä */
       END. /* f2 */

       else if nap = "f3" or nap = "3" THEN DO: /* nimihaku */
          cfc = "haku". RUN Syst/ufcolor.p.
          ha-nimi = "".
          UPDATE ha-nimi WITH FRAME syha3.
          HIDE FRAME syha3.
          IF ha-nimi <> "" THEN DO:
             /* haetaan nimeä */

             FIND FIRST TMSUser where INDEX(TMSUser.UserName,ha-nimi) > 0 and LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
             USE-INDEX UserName no-lock no-error.
             IF NOT AVAILABLE TMSUser THEN DO:
                FIND FIRST TMSUser where TMSUser.UserName > ha-nimi and LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
                USE-INDEX UserName no-lock no-error.
                IF NOT AVAILABLE TMSUser THEN DO:
                   message "NOT FOUND".
                   cfc = "sel". RUN Syst/ufcolor.p.
                   NEXT BROWSE.
                END.
             END.
             /*  Name was found */
             memory = recid(TMSUser).
             must-print = TRUE.
             order = 2.
             cfc = "sel". RUN Syst/ufcolor.p.
             NEXT BROWSE.
          END. /* nimihaku */
       END. /* f3 */

       else if nap = "4" or nap = "f4" THEN DO:
          IF lcExclGroup = "" THEN lcExclGroup = "NOTinUSE".
          ELSE lcExclGroup = "".
          CLEAR FRAME sel ALL no-pause.
          RUN local-find-first.
          ASSIGN
             memory = recid(TMSUser)
             must-print = TRUE 
             cfc = "sel".
          RUN Syst/ufcolor.p.
          NEXT BROWSE.
       END.
       /* previous line */
       else if nap = "cursor-up" THEN DO:
          IF FRAME-LINE = 1 THEN DO:
             FIND TMSUser where recid(TMSUser) = rtab[FRAME-LINE] no-lock.
             RUN local-find-prev. 
             IF NOT AVAILABLE TMSUser THEN DO:
                BELL.
                message "This is the 1st row !".     
                NEXT BROWSE.
             END.

             scroll DOWN.
             DO i = FRAME-DOWN TO 2 BY -1:
                rtab[i] = rtab[i - 1].
             END.
             rtab[1] = recid(TMSUser).

             DISPLAY 
                TMSUser.UserCode 
                TMSUser.UserName 
                TMSUser.Initials
                TmsUser.UserGroup 
                TMSUser.foreignid
                TMSUser.fromdate 
                TMSUser.Todate.
          END.
          ELSE up 1.
       END.

       /* NEXT line */
       else if nap = "cursor-down" THEN DO:
          IF FRAME-LINE = FRAME-DOWN THEN DO:

             FIND TMSUser where recid(TMSUser) = rtab[FRAME-LINE] no-lock.
              RUN local-find-next. 
              IF NOT AVAILABLE TMSUser THEN DO:
                BELL.
                message "This is the last row !".
                NEXT BROWSE.
             END.
             scroll up.
             DO i=1 TO FRAME-DOWN - 1:
                rtab[i] = rtab[i + 1].
             END.
             rtab[FRAME-DOWN] = recid(TMSUser).

             DISPLAY 
               TMSUser.UserCode 
               TMSUser.UserName 
               TMSUser.Initials
               TMSUser.UserGroup 
               TMSUser.foreignid
               TMSUSer.Fromdate 
               TMSUSer.toDate.
          END.
          ELSE DOWN 1.
       END.

       /* previous page */
       else if lookup(nap,"page-up,prev-page,-") > 0 THEN DO:
          memory = rtab[1].
          FIND TMSUser where recid(TMSUser) = memory no-lock.

          /* peruutetaan edell. sivulle */
          DO i = 1 TO frame-down(sel).
             RUN local-find-prev.
             IF AVAILABLE TMSUser THEN memory = recid(TMSUser).
             ELSE DO:
                /* jos ei lOytynyt yhtAAn */
                IF i = 1 THEN DO:
                   BELL.
                   message "This is the first page !".   
                   PAUSE 1 no-message. HIDE MESSAGE.
                   NEXT BROWSE.
                END.
                ELSE LEAVE.
             END. /* NOT AVAILABLE */
          END.
          must-print = TRUE.
          NEXT BROWSE.
       END.

       /* NEXT page */
       else if lookup(nap,"page-down,next-page,+") > 0 THEN DO WITH FRAME sel:
          IF rtab[FRAME-DOWN] = ? THEN DO:
             BELL.
             message "This is the last page !".
             NEXT BROWSE.
          END.
          ELSE DO:
             memory = rtab[FRAME-DOWN].
             must-print = TRUE.
             NEXT BROWSE.
          END.
       END.

       else if nap = "5" or nap = "f5" AND lcRight = "RW" THEN DO :  
       /* lisAys */
          must-add = TRUE.
          NEXT BROWSE.
       END.

       else if nap = "3" or nap = "f3" AND lcRight = "RW" THEN DO:
          FIND TMSUser where recid(TMSUser) = rtab[FRAME-LINE] no-lock.
          RUN Syst/usersman.p (TMSUser.UserCode).
          
          ufkey = TRUE.
          NEXT BROWSE.
          
       end.
              /* removal */
       else if nap = "6" or nap = "f6" AND lcRight = "RW" THEN DO TRANS:
          FIND TMSUser where recid(TMSUser) = rtab[FRAME-LINE]
          exclusive-lock.
          COLOR DISPLAY value(ctc) 
             TMSUser.UserCode 
             TMSUser.UserName 
             TMSUser.Initials
             TMSUser.Usergroup
             TMSUSer.fromdate
             TMSUSer.todate
             TMSuser.foreignid.
          i = 0.
          IF FRAME-LINE = 1 THEN i = 1.
          memory = rtab[1 + i].
          ASSIGN ok = FALSE.
          message "ARE YOU SURE YOU WANT TO REMOVE (Y/N)? " UPDATE ok.
          COLOR DISPLAY value(ccc) 
             TMSUser.UserCode 
             TMSUser.UserName 
             TMSUser.Initials
             TMSUser.usergroup
             TMSUser.foreignid
             TMSUSer.fromdate
             TMSUSer.todate.
          IF ok THEN DO:
             firstline = FRAME-LINE - 1.

             /* check if limits amounts is defined individualy  */
            FIND FIRST UserLimit WHERE 
                       UserLimit.Brand = gcBrand AND 
                       UserLimit.LimitTarget = "TMSUser" AND
                       UserLimit.LimitTargetID = TMSUser.UserCode NO-LOCK NO-ERROR.
             IF AVAIL UserLimit THEN DO:
                MESSAGE "Operation canceled: please remove first all defined user limits! "
                VIEW-AS ALERT-BOX TITLE "INFO".
                must-print = TRUE.
                memory = recid(TMSUser). 
                NEXT BROWSE.
             END.
            

             IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTMSUser).
             DELETE TMSUser.
             must-print = TRUE.
             NEXT BROWSE.
          END.
       END.
     
     else  if nap = "enter" or nap = "return" THEN DO:
      FIND TMSUser where recid(TMSUser) = rtab[FRAME-LINE] NO-LOCK.
      PAUSE 0 no-message.
            
     SHOW-USER:
       REPEAT WITH FRAME lis ON ENDKEY UNDO SHOW-USER, LEAVE SHOW-USER.

            moremail = SUBSTRING(TMSUser.Email,51,50).

            DISPLAY 
            TMSUser.UserCode  
            TMSUser.UserName 
            TMSUser.Initials 
            "SECRET" @ TMSuser.password
            TMSUSer.UserGroup
            TMSUser.AdminUser
            tmsuser.foreignid
            TMSUSer.fromdate
            TMSUSer.todate
            RepDir 
            TMSUser.brand 
            TMSUser.StartMenu 
           /*  TMSUser.CreditLimit */
            TMSUser.Email 
            moremail 
          WITH FRAME lis.

          ASSIGN ehto   = 0
                ufk    = 0            
                ufk[1] = 7  WHEN lcRight = "RW"  
                ufk[3] = 26 WHEN lcRight = "RW" 
                ufk[8] = 8.
             
         RUN Syst/ufkey.p.
 
          IF toimi = 1 AND lcRight = "RW" THEN DO: /* update it */
             
             ehto = 9. RUN Syst/ufkey.p.

             IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMSUser).
             
             FIND CURRENT TMSUser EXCLUSIVE-LOCK.

             RUN pUpdateUser.

    edEMail: REPEAT WITH FRAME lis:
             IF moremail NE "" THEN
                ASSIGN
                TMSUser.Email = SUBSTRING(TMSUser.Email,1,50) + moremail
                moremail      = "".
             fIsEmail(TMSUser.Email, OUTPUT ret).
             IF ret NE 1 AND TMSUser.Email NE "" THEN DO:
                BELL.
                MESSAGE "Invalid E-mail address line!".  PAUSE. 
                UPDATE TMSUser.Email moremail.
                NEXT edemail.
             END.
             ELSE LEAVE edemail. 
            END. /*edemail*/     

             IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMSUser).
          END.
          
          IF toimi = 3 THEN DO:
             RUN Syst/adduserlimitcui.p ("TMSUser", TMSUser.UserCode).
          END.

          IF toimi = 8 THEN LEAVE.

       END.
       
        DISP TMSUser.UserName TMSUser.Initials TMSUser.StartMenu
        WITH FRAME sel.
        ufkey = TRUE.
        NEXT BROWSE.
       
      END.

       else if lookup(nap,"home,h") > 0 THEN DO:
          RUN local-find-first.
          ASSIGN
          memory = recid(TMSUser)
          must-print = TRUE.
       END.

       else if lookup(nap,"end,e") > 0 THEN DO:
          RUN local-find-last.    
          ASSIGN
          memory = recid(TMSUser)
          must-print = TRUE.
       END.

       else if nap = "8" or nap = "f8" THEN DO:
          HIDE FRAME sel no-pause.
          RETURN.
       END.
    END.  /* BROWSE */

HIDE FRAME sel no-pause.
HIDE MESSAGE.

fCleanEventObjects().


PROCEDURE pUpdateUser:

   DEF VAR lcOldGroup AS CHAR NO-UNDO.
   
   lcOldGroup = TMSUser.UserGroup.

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
            UPDATE
               tmsuser.foreignid
               TMSUser.UserName 
               TMSUser.Initials 
               TMSUser.Password WHEN NEW TMSUser  
               TMSUser.UserGroup 
               TMSUser.AdminUser WHEN llAdminUser
               TMSUser.Fromdate
               TMSUser.Todate
               RepDir  
               tmsuser.brand 
               TMSUser.StartMenu
              /* TMSUser.CreditLimit */
               TMSUser.Email 
               moremail 
            WITH FRAME lis.
             
            FIND FIRST UserGrp WHERE UserGrp.UserGroup = TMSUser.UserGroup
               NO-LOCK NO-ERROR.
            IF NOT AVAILABLE UserGrp THEN DO:
               MESSAGE "Unknown user group"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.

            /* if group contains even one admin level token with modify
               right then only admin level user can be included to it */
            IF fIsAdminToken(UserGrp.ModifyTokens) AND 
               NOT TMSUser.AdminUser 
            THEN DO:
               TMSUser.UserGroup = lcOldGroup.
               MESSAGE "Only admin level user can be assigned to an"
                       "admin level group"
               VIEW-AS ALERT-BOX INFORMATION.
               NEXT. 
            END.
                  
            LEAVE.
   END.

END PROCEDURE.

PROCEDURE local-find-first:

    IF order = 1 THEN FIND FIRST TMSUser USE-INDEX UserCode WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
               no-lock no-error.
    ELSE IF order = 2 THEN FIND FIRST TMSUser USE-INDEX UserName WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
              no-lock no-error.
    ELSE IF order = 3 THEN FIND FIRST TMSUser USE-INDEX ForeignID WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
              no-lock no-error.
    ELSE  FIND FIRST TMSUser WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 no-lock no-error.

END PROCEDURE.

PROCEDURE local-find-next:

    IF order = 1 THEN FIND NEXT TMSUser USE-INDEX UserCode WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
               no-lock no-error.
    ELSE IF order = 2 THEN FIND NEXT TMSUser USE-INDEX UserName WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
               no-lock no-error.
    ELSE IF order = 3 THEN FIND NEXT TMSUser USE-INDEX ForeignId WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
               no-lock no-error.
    ELSE  FIND NEXT TMSUser WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 no-lock no-error.
   
END PROCEDURE.

PROCEDURE local-find-prev:

    IF order = 1 THEN FIND prev TMSUser USE-INDEX UserCode WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
               no-lock no-error.
    ELSE IF order = 2 THEN FIND prev TMSUser USE-INDEX UserName WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
               no-lock no-error.
    ELSE IF order = 3 THEN FIND prev TMSUser USE-INDEX ForeignID WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
               no-lock no-error.
    ELSE FIND prev TMSUser WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 no-lock no-error.


END PROCEDURE.

PROCEDURE local-find-last:

    IF order = 1 THEN FIND LAST TMSUser USE-INDEX UserCode WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
               no-lock no-error.
    ELSE IF order = 2 THEN FIND LAST TMSUser USE-INDEX UserName WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
               no-lock no-error.
    ELSE IF order = 3 THEN FIND LAST TMSUser USE-INDEX ForeignID WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 
               no-lock no-error. 
     ELSE FIND last TMSUser WHERE LOOKUP(TMSUser.UserGroup,lcExclGroup) = 0 no-lock no-error.

END PROCEDURE.




