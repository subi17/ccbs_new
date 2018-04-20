/* ---------------------------------------------------------------------------
  MODULE .......: cancel_neba_order
  FUNCTION .....: Handle NEBA permamency handling when order is cancelled
                  before fixed line installation.
                  Actions:
                  -Create customer for billing if needed
                  -Create NEBA fee for the cancelled order
  APPLICATION ..: TMS
  CREATED ......: 20.4.2018
  MODIFIED .....: 
  VERSION ......: yoigo
  -------------------------------------------------------------------------- */
DEF INPUT  PARAMETER iiOrder  AS INT  NO-UNDO.



