/* ---------------------------------------------------------------------------
  MODULE .......: PAYMENTB.P
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for handling payments            
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 10.05.07
  CHANGED ......: 
  Version ......: Yoigo
  ------------------------------------------------------------------------- */


/* read payment files */
RUN Ar/readpaymb.p.

/* make payments to invoices that have been sent to bank for direct debiting */
RUN Ar/ddpaymentb.p.

QUIT.

