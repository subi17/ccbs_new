/* ufcolor.p *

saa:    framen nimen common-muuttujassa cfc
antaa:  formin vArit common-muuttujassa cfc
   titlen vArit common-muuttujassa ctc */
{Syst/commali.i}


if opsys = "msdos" THEN DO:
   IF Syst.CUICommon:yvari THEN DO:
      FIND FColor where FColor.FrameName = Syst.CUICommon:cfc no-lock no-error.
      IF NOT AVAILABLE FColor THEN DO:
    message "VArimAAritys puuttuu, frame " + Syst.CUICommon:cfc.
    PAUSE 2 no-message.
      END.
      ELSE ASSIGN Syst.CUICommon:cfc = FColor.FrameColor Syst.CUICommon:ctc = FColor.TitleColor.
   END.
END.

if not available FColor or opsys <> "msdos" OR NOT Syst.CUICommon:yvari THEN DO:
   ASSIGN
   Syst.CUICommon:cfc = "normal"
   Syst.CUICommon:ctc = "messages".
END.
PAUSE 0.

