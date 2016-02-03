/* ----------------------------------------------------------------------
  MODULE .......: TMSLOGIN.p
  TASK .........: Login into TMS
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 13.05.02
  CHANGED ......: 01.09.03 jp  /TMS+
                  29.10.03 jp  ask password
                  03.03.05 kl  return correctly
                  02.11.05 mvi dont allow login for username in NOTinUSE group
                  15.09.06 aam don't let through with ctrl-e
                  11.01.07 mvi password expiration, stored in TMSPass
                  28.04.07 mvi fixed bug in asking passwd, error occured 
                               if user had empty password in TMSUser.
                  02.06.07 mvi Yoigo version, removed .p from RUN commands 
                  02.06.07 mvi Usergrp.PasswordExpires checked in login

  TODO..........: ask first both username and password then check them
                  so usernames cannot be guessed
  VERSION ......: XFera 
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
{Func/date.i}

DEFINE VARIABLE liPassValidDays AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPassNotifyDays AS INTEGER NO-UNDO. 
DEFINE VARIABLE liTempDate AS DATE NO-UNDO.
DEFINE VARIABLE liTempTime AS INTEGER NO-UNDO.
DEFINE VARIABLE olPasswordChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE llChangePasswordNow AS LOGICAL NO-UNDO. 
DEFINE VARIABLE liLoop AS INTEGER NO-UNDO. 

/* use brand 1 to get tmsparams, will be selected later */
gcBrand = "1".
{Func/tmsparam.i PassWdValidDays} liPassValidDays = TMSParam.IntVal.
{Func/tmsparam.i PassWdExpireNotify} liPassNotifyDays = TMSParam.IntVal.

form
   skip(1)
   katun  label   "  User Id ....." 
   help "Enter a valid TMS User ID  (EMPTY ID: QUIT)" TmsUser.UserName no-label 
   skip
   passwd FORMAT "X(16)" label   "  Password ... " blank
   skip(1)
   with 
     row 8 
     centered 
     title color value(ctc) " MULTI-BRAND TMS: USER LOGIN "
     side-labels color value(cfc) overlay frame login.


do with frame login:

   pause 0 no-message. 

   IF ergo-kbd THEN display with frame f_code-ERGO.
               ELSE display with frame f_code.

   cfc = "lis". run ufcolor.
   pause 0 no-message.

   ehto = 9. run ufkey.
   assign si-pvm = pvm.

   if katun = "" then do:
      cfc = "tunnus". run ufcolor.

      input through value("who am i").

      import unformatted katun.
      input close.
      katun  = ENTRY(1,katun," ").
      if entry(1,katun," ") = "starnet" then Katun = "eka".

      disp katun with frame login.

      USER_LOGIN: /* ASK USER ID AND PASSWORD */
      repeat with frame login on endkey undo USER_LOGIN, RETURN:
            update katun
                   validate (input katun = "" or 
                   can-find(FIRST TmsUser where 
                                  TmsUser.UserCode = INPUT katun AND
                                  TmsUser.UserGroup NE "NOTinUSE"),
                             "UNKNOWN USER ID - PLEASE CHECK AND RETYPE !").

            if katun = "" then quit.

            find TmsUser where  
                 TmsUser.UserCode = katun no-lock no-error.

            disp TmsUser.UserName.
            PAUSE 0.

            /* find group for expiration check */
            FIND FIRST UserGrp NO-LOCK WHERE
                       UserGrp.UserGroup = TmsUser.UserGroup
                       NO-ERROR.
            IF NOT AVAIL UserGrp THEN DO:
               MESSAGE "UKNOWN USERGROUP!".
               BELL.
               CLEAR FRAME login.
               NEXT USER_LOGIN.
            END.

            /* copy user id into the common variables */
            assign
              kanro       = TmsUser.userNum
              karyhma     = TmsUser.UserGroup
              ergo-kbd    = TmsUser.ErgoKeyb
              gcAllBrand  = (IF INDEX("*",TMSUser.brand) > 0 THEN TRUE 
                             ELSE FALSE).


            /* get users current password */ 
            FIND FIRST tmspass NO-LOCK WHERE 
                       tmspass.usercode = tmsuser.usercode NO-ERROR.
            IF NOT AVAIL tmspass THEN DO:
               MESSAGE "User has no password!" VIEW-AS ALERT-BOX.
               QUIT.
            END.

            if tmspass.password <> "" then do:
               update passwd.
               
               if passwd <> TMSPass.PassWord then do:
                  message "INVALID ID !".
                  bell. 
                  clear frame login. 
                  next USER_LOGIN.
               end.
            end. /* passwd */
           
            /* compare password lenghts (EQ test ignores trailing blanks) */   
            IF LENGTH(passwd) NE LENGTH(tmspass.password) THEN DO:
               MESSAGE "INVALID ID !".
               BELL.
               CLEAR FRAME login.
               NEXT USER_LOGIN.
            END.
            
            /* go thru char by char, otherwise not case sensitive */
            DO liLoop = 1 TO LENGTH(passwd):
               IF ASC(SUBSTRING(passwd,liLoop,1)) NE 
                  ASC(SUBSTRING(tmspass.password,liLoop,1)) THEN DO:
                     MESSAGE "INVALID ID !".
                     BELL.
                     CLEAR FRAME login.
                     NEXT USER_LOGIN.
               END.
            END.
           
            
            /* check if password has been changed by someone else */
            IF tmspass.creator NE tmsuser.usercode THEN DO:
               MESSAGE "Administrator has changed your password." SKIP
                  "You must specify a new password now." 
                  VIEW-AS ALERT-BOX TITLE " PASSWORD EXPIRED ".
               RUN chpasswd(OUTPUT olPasswordChanged).
               IF NOT olPasswordChanged THEN DO:
                  MESSAGE "Password not changed. Please log in again!"
                     VIEW-AS ALERT-BOX INFO.
               END.
               NEXT USER_LOGIN. 
            END.
            
            /* if usergroup's password can expire */
            IF UserGrp.PasswordExpires THEN DO:

               /* check if password has expired */ 
               fSplitTS(TMSPass.CreateTS,liTempDate,liTempTime).
               IF TODAY - liTempDate > liPassValidDays THEN DO:
                  /* force user to change password */
                  RUN chpasswd(OUTPUT olPasswordChanged).
                  IF NOT olPasswordChanged THEN DO:
                     MESSAGE "Password not changed. Please log in again!"
                        VIEW-AS ALERT-BOX INFO.
                  END.
                  NEXT USER_LOGIN. 
               END.
              
               /* shall we notify of old passwords? */
               IF liPassNotifyDays > 0 AND
                  liPassValidDays - (TODAY - liTempDate) < liPassNotifyDays
               THEN DO:
                  /* notify user that the password is about to expire */
                  MESSAGE "Your password will expire in"
                     liPassValidDays - (TODAY - liTempDate) "days." SKIP
                     "Do you want to change it now?" VIEW-AS ALERT-BOX  
                     BUTTONS YES-NO UPDATE llChangePasswordNow.
                  
                  IF llChangePasswordNow THEN DO:
                     RUN chpasswd(OUTPUT olPasswordChanged).   
                     IF NOT olPasswordChanged THEN DO:
                        MESSAGE "Password not changed. Please log in again!"
                           VIEW-AS ALERT-BOX INFO.
                     END.
                     NEXT USER_LOGIN. 
                  END.   

               END.
            END.        
                       
            pause 0.
            leave.
      end. /* USER_LOGIN */

      if katun = "" then quit. 

   end. /* login */

   hide frame login.

end.

/* select the brand and start TMS with appropiate databases */
do while true:
    run brandsel.
    IF RETURN-VALUE = "LEAVE" THEN LEAVE.
end.    

IF VALID-HANDLE(ghFunc1)   THEN DELETE OBJECT ghFunc1.

QUIT.
