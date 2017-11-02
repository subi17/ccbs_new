/* ---------------------------------------------------
  MODULE .......: NNinfo.p
  FUNCTION .....: Show information about a menu item on screen
  APPLICATION ..: TICKET MASTER
  CREATED ......: 19.08.98
  MODIFIED .....: 11.11.02 jr Eventlog
  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER MenuId AS c NO-UNDO.

DEF VAR xfk AS i EXTENT 8.
DEF VAR i   AS i.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMenuTree AS HANDLE NO-UNDO.
   lhMenuTree = BUFFER MenuTree:HANDLE.
   RUN StarEventInitialize(lhMenuTree).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMenuTree).
   END.
END.

DO i = 1 TO 8:
   xfk[i] = Syst.Var:ufk[i].
END.
ASSIGN Syst.Var:ufk = 0 Syst.Var:ufk[1] = 2 Syst.Var:ufk[8] = 8. Syst.Var:ehto = 0.

FIND MenuTree where MenuTree.MenuId = MenuId no-lock.

form
   MenuTree.Memo[1 FOR 15] NO-LABEL 
WITH OVERLAY ROW 2 centered
   title " (" + MenuTree.MenuId + ") " + MenuTree.MenuTitle + " (" + 
   (if MenuTree.Module = "" then "MENU" ELSE MenuTree.Module)
   + ") " FRAME info.

PAUSE 0.            
DISP MenuTree.Memo[1 FOR 15] WITH FRAME info.

RUN Syst/ufkey.p.

IF Syst.Var:toimi = 1 THEN DO TRANS:
   PAUSE 0.
   Syst.Var:ehto = 9. RUN Syst/ufkey.p.
   FIND MenuTree where MenuTree.MenuId = MenuId exclusive-lock.
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMenuTree).
   UPDATE MenuTree.Memo[1 FOR 15] WITH FRAME info.
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMenuTree).
END.   

HIDE FRAME info  no-pause.

DO i = 1 TO 8.
   Syst.Var:ufk[i] = xfk[i].
END.
Syst.Var:ehto = 3.
RUN Syst/ufkey.p.
PAUSE 0.
