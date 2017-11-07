/* ----------------------------------------------------------------------------
  MODULE .......: TARIFFC.P
  FUNCTION .....: call tariff.p from menu
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 24-09-02
  MODIFIED .....: 04.04.03 kl RUN Mc/tariff,.p new parameter
                  26.06.03 kl RUN Mc/tariff,.p new parameter
                  04.07.03 kl RUN Mc/tariff,.p new parameter

  VERSION ......: M15
----------------------------------------------------------------------------*/

{Syst/commali.i}

/* call tariff with CCN 0 -> all */
RUN Mc/tariff.p(0,0,"",0,"",0).
