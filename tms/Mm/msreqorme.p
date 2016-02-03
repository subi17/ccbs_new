/*-----------------------------------------------------------------------------
  MODULE .......: msreqorme
  FUNCTION .....: ms request menu, order request type 13
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 28.03.06
  changePVM ....: 16.04.07/aam new parameters to tmscodesbr
                  31.10.07 jp  new parameter for msrequest
                  
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}
RUN reqstatmenu.p({&REQTYPE_SUBSCRIPTION_CREATE}).
