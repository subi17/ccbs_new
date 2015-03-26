/*------------------------------------------------------------------
  MODULE .......:  UVIIK.P
  KUTSUVA MODULI:  MIKA TAHANSA
  FUNCTION .....:  LASKEE VIIKON NUMERON ANNETUSTA PAIVAMAARASTA
  APPLICATION ..:  OSTONIEKKA
  AUTHOR .......:  PK
  CREATED ......:  26.04.1989

  changePVM  VERSIO TEKI SELITE
  ---------  ------ ---- ------
  26.04.89   0.4/0      pk   EnsimmAinen versio

  ------------------------------------------------------------------*/

/* seuraavat kaksi muuttujaa mAAriteltAvA kutsuvassa NEW shared muodossa */
DEF shared VAR uvpvm AS Date. /* pAivAmAArA sisAAn */
def shared var uviikko as int format "999999".


DEF VAR apupv LIKE uvpvm.
DEF VAR viikonp AS INT. /* viikonpAivA */
DEF VAR ero AS INT.
/* seuraavassa on taulukko josta nAhdAAn montako pAivAA vuoden eka pAivAAn
on lisAttAvA, jotta saadaan 1. viikon 1.pAivAn pAivAmAArA */
DEF VAR t AS INT EXTENT 7 initial [1,0,-1,-2,-3,3,2].
/* tarkistetaan ettei ole tuntematon */
IF uvpvm = ?
THEN DO:
    uviikko = 0.
    RETURN.
END.

/* vuoden eka pAivAn viikonpAivA */
apupv = date(1,1,year(uvpvm)).
viikonp = weekday(apupv).
/* ko. viikon ens. pAivAn pAivAys */

apupv = apupv + t[viikonp].
/* annetun pAivAmAArAn ja lasketun pAivAn erotus */
ero = uvpvm - apupv.
IF ero >= 0    /* on saman vuoden viikko tai seuraavan vuoden eka viikko */
THEN DO:
    uviikko = 100 * year(uvpvm) + truncate(ero / 7,0) + 1.

    /* voiko olla seuraavan vuoden eka viikko */
    IF truncate(ero / 7,0) + 1 >= 53
    THEN DO:
   /* vuoden viimeisen pAivAn pAivAmAArA */
   viikonp = weekday(date(12,31,year(uvpvm))).
   /* ma,ti,ke --> viikko = 1 seur.vuonna */
   IF viikonp > 1 AND viikonp < 5
   THEN uviikko = 100 * year(uvpvm) + 101.
   /* yksi vuosi lisAttiin viikko aina 01 */
    END.
END.
ELSE DO:   /* edellisen vuoden viimeinen viikko */
    /* kutsutaan rekursiivisesti */
    /* alkuperAinen talteen ettei kutsuvassa sekoile */
    apupv = uvpvm.
    uvpvm = date(12,31,year(uvpvm) - 1).
    RUN uviik.p.
    uvpvm = apupv.
END.

