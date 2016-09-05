/*------------------------------------------------------
  MODULE .......: VPLAHA.P
  FUNCTION .....: Laskut hakurutiini
  SOVELLUTUS ...: VP
  AUTHOR .......: TT
  CREATED ......: 12.07.1991
  changePVM ....: 13.10.1998 pt in English
                  15.09.03/aam brand
                  15.04.04/aam index CustName replaced with CustNum
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR order AS INT NO-UNDO.

DEF VAR nhaku     LIKE Invoice.InvNum    NO-UNDO.
def var thaku     as char format "x(30)" NO-UNDO.
DEF VAR liCustNum AS INT                 NO-UNDO.

form
   nhaku
   help "Give number of an invoice"
   with title color value(ctc) " INVOICE No. "
   ROW 2 col 2 COLOR value(cfc) NO-LABELS
   OVERLAY FRAME asno.

form
    liCustNum FORMAT ">>>>>>>9"
    help "Give customer's number"           
    WITH ROW 2 col 2 TITLE
    color value(ctc) " CUSTOMER'S Number "
    NO-LABELS COLOR value(cfc) OVERLAY FRAME nimi.

    assign cfc = "puyr". RUN Syst/ufcolor.p.
    PAUSE 0 no-message.

LOOP:
repeat:
    /* haku laskunumerolla */
    IF order = 1 THEN DO WITH FRAME asno:
       ASSIGN nhaku = 0.
       ehto = 9. RUN Syst/ufkey.p.
       UPDATE nhaku.
       HIDE FRAME asno.
       IF nhaku NE 0 THEN DO:
          FIND FIRST Invoice where 
                     Invoice.Brand  = gcBrand AND
                     Invoice.InvNum <= nhaku
          USE-INDEX InvNum no-lock no-error.

          IF AVAILABLE Invoice THEN ASSIGN si-recid = recid(Invoice).
          ELSE DO:
             message "CAN'T FIND !".
             si-recid = ?.
             BELL.
             PAUSE 1 no-message.
          END.
       END.
       ELSE ASSIGN si-recid = ?.
       HIDE FRAME asno no-pause.
       LEAVE LOOP.
    END.

    /* haku asiakas */
    ELSE IF order = 2 THEN DO WITH FRAME nimi:
       liCustNum = 0.
       ehto = 9. RUN Syst/ufkey.p.
       UPDATE liCustNum.
       HIDE FRAME nimi.
       if liCustNum > 0 THEN DO:
          FIND FIRST Invoice where 
                     Invoice.Brand  = gcBrand AND
                     Invoice.CustNum >= liCustNum
          no-lock no-error.

          IF AVAILABLE Invoice THEN ASSIGN si-recid = recid(Invoice).
          ELSE DO:
             message "CAN'T FIND !".
             si-recid = ?.
             BELL.
             PAUSE 1 no-message.
          END.
       END.
       ELSE ASSIGN si-recid = ?.
       HIDE FRAME nimi no-pause.
       LEAVE LOOP.
    END.

END. /* LOOP */

