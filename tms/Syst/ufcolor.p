/* ufcolor.p *

saa:    framen nimen common-muuttujassa cfc
antaa:  formin vArit common-muuttujassa cfc
   titlen vArit common-muuttujassa ctc */
{Syst/commali.i}


if opsys = "msdos" THEN DO:
   IF yvari THEN DO:
      FIND FColor where FColor.FrameName = cfc no-lock no-error.
      IF NOT AVAILABLE FColor THEN DO:
    message "VArimAAritys puuttuu, frame " + cfc.
    PAUSE 2 no-message.
      END.
      ELSE ASSIGN cfc = FColor.FrameColor ctc = FColor.TitleColor.
   END.
END.

if not available FColor or opsys <> "msdos" OR NOT yvari THEN DO:
   ASSIGN
   cfc = "normal"
   ctc = "messages".
END.
PAUSE 0.

