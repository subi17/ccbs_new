/* -----------------------------------------------
  MODULE .......: CHPASS.P
  FUNCTION .....: Allow user to change his/her own password
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 25.03.03
  MODIFIED .....: 11.01.07 mvi / new rules to check password, use TMSPass
                  02.06.07 mvi / Yoigo version
  Version ......: Yoigo
  ------------------------------------------------------ */

{Syst/commali.i}

DEFINE VARIABLE olPasswordChanged AS LOGICAL NO-UNDO.


RUN Syst/chpasswd(OUTPUT olPasswordChanged).

IF NOT olPasswordChanged THEN 
   MESSAGE " Password not changed!" VIEW-AS ALERT-BOX INFO.
