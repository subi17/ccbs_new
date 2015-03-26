/*------------------------------------------------------
  MODULE .......: UFXKey.P
  FUNCTION .....: DISPLAY SINGLE MENU Key  
  APPLICATION ..: NN            
  AUTHOR .......: KL
  CREATED ......: 30.06.98  
  MODIFIED .....:                
  Version ......: M15
  ------------------------------------------------------ */

DEF INPUT PARAMETER m-place AS i NO-UNDO.
DEF INPUT PARAMETER m-num   AS i NO-UNDO.

def var mnu as c format "x(8)" EXTENT 16. 
DEF VAR pos AS i NO-UNDO.

PAUSE 0.

/* DEFINE place FOR the menu */
pos = m-place * 9 - 8.
IF m-place >= 5 THEN pos = pos + 8.

/* single Key form */
form 
   mnu[m-place] SKIP
   mnu[m-place  + 8]
WITH no-box NO-LABELS ROW 20 column pos OVERLAY FRAME m_frm.

COLOR DISPLAY MESSAGE mnu[m-place] mnu[m-place + 8] WITH FRAME m_frm.

FIND MenuText where MenuNum = m-num no-lock no-error.
IF AVAILABLE MenuText THEN
   ASSIGN mnu [m-place]     = substring(MenuText,1,8)
          mnu [m-place + 8] = substring(MenuText,9).
ELSE
   assign mnu [m-place]     = string(m-num) + " ?"
          mnu [m-place + 8] = "".

DISPLAY mnu[m-place] mnu[m-place + 8] WITH FRAME m_frm.

