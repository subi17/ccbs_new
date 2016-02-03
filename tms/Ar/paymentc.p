/* ----------------------------------------------------------------------------
  MODULE .......: PAYMENTC.P
  FUNCTION .....: call payments.p from menu
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 25-10-02
  MODIFIED .....: 22.03.07 kl  new param for run payments

  VERSION ......: M15
----------------------------------------------------------------------------*/

{Syst/commali.i}

/* call payments with customer 0 -> all */
RUN payments.p (0,0,"").
