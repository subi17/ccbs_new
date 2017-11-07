/* ufcolor.p *

saa:    framen nimen common-muuttujassa cfc
antaa:  formin vArit common-muuttujassa cfc
   titlen vArit common-muuttujassa ctc */
{Syst/commali.i}


if opsys = "msdos" THEN DO:
   IF Syst.Var:yvari THEN DO:
      FIND FColor where FColor.FrameName = Syst.Var:cfc no-lock no-error.
      IF NOT AVAILABLE FColor THEN DO:
    message "VArimAAritys puuttuu, frame " + Syst.Var:cfc.
    PAUSE 2 no-message.
      END.
      ELSE ASSIGN Syst.Var:cfc = FColor.FrameColor Syst.Var:ctc = FColor.TitleColor.
   END.
END.

if not available FColor or opsys <> "msdos" OR NOT Syst.Var:yvari THEN DO:
   ASSIGN
   Syst.Var:cfc = "normal"
   Syst.Var:ctc = "messages".
END.
PAUSE 0.

