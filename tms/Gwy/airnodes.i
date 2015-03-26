/* ----------------------------------------------------------------------
  MODULE .......: air.i
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 09.11.12
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
&IF "{&AIRNODES_I}" NE "YES" 
&THEN
&GLOBAL-DEFINE AIR_NODES YES

{commali.i}
{cparam2.i}

FUNCTION fGetAIRNode RETURNS CHAR:

   DEF VAR lcURLParam AS CHAR NO-UNDO EXTENT 3.
   DEF VAR lcURL AS CHAR NO-UNDO. 
   DEF VAR liNode AS INT NO-UNDO.
   DEF VAR liNum AS INT NO-UNDO. 
   DEF VAR lcNodes AS CHAR NO-UNDO.

   lcNodes = fCParam("ATM","Nodes").

   IF INDEX(lcNodes,"A") > 0 THEN ASSIGN
      liNum = liNum + 1
      lcURLParam[liNum] = "urlATMa".
   IF INDEX(lcNodes,"B") > 0 THEN ASSIGN
      liNum = liNum + 1
      lcURLParam[liNum] = "urlATMb".
   IF INDEX(lcNodes,"C") > 0 THEN ASSIGN
      liNum = liNum + 1
      lcURLParam[liNum] = "urlATMc".

   IF liNum EQ 0 THEN ASSIGN 
      liNum = 1
      lcURLParam[liNum] = "urlATMa".

   liNode = TIME MOD liNum + 1.
   lcUrl = fCParam("URL",lcURLParam[liNode]).
   
   RETURN lcUrl.

END FUNCTION.

&ENDIF
