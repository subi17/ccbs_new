/* ----------------------------------------------------------------------------
  MODULE .......: ROAMTARIFFLISTC.P
  FUNCTION .....: Calls roamtarifflist.p from menu.
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 20.7.2007
  MODIFIED .....: 
  VERSION ......: xfera
----------------------------------------------------------------------------*/

{commali.i}

/* call roamtariffilist with plmn "" -> all */
RUN roamtarifflist("").
