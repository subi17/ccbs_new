USING nl.pug.stomp.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS nl.pug.stomp.TekstBericht INHERITS Bericht IMPLEMENTS javax.jms.TextMessage:
  DEFINE PRIVATE VARIABLE berichtTekst AS LONGCHAR NO-UNDO. 
 
  DEFINE PUBLIC PROPERTY text AS LONGCHAR NO-UNDO
  GET:
    RETURN berichtTekst.
  END GET.
  SET(tekst AS LONGCHAR):
    berichtTekst = tekst.
  END SET.

  CONSTRUCTOR PUBLIC TekstBericht(tekst AS LONGCHAR):
    SUPER().
    THIS-OBJECT:text = tekst.
  END CONSTRUCTOR.

END CLASS.
