/* -------------------------------------------------------
  MODULE .......: NNCCIT.P
  FUNCTION .....: Calculate number of items in a CF
  APPLICATION ..: NN
  AUTHOR .......: pt
  CREATED ......: 03-12-98
  MODIFIED .....: 
  Version ......: M15
  --------------------------------------------------------- */

  DEF INPUT PARAMETER FFNum AS i.

  DEF OUTPUT PARAMETER amt-billed    AS i NO-UNDO.
  DEF OUTPUT PARAMETER amt-nonbilled AS i NO-UNDO.

  amt-billed    = 0.
  amt-nonbilled = 0.

  FOR EACH FFItem no-lock where FFItem.FFNum = FFNum:
     IF Billed THEN amt-billed    = amt-billed    + 1.
     ELSE           amt-nonbilled = amt-nonbilled + 1.
  END.
