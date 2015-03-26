/* ----------------------------------------------------------------------
  MODULE .......: Msrequestm
  TASK .........: call msrequest from menu
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 16.12.04
  CHANGED ......: 12.12.06 mvi added 2nd param ? to run msrequest        
                  31.10.07 jp  new parameter for msrequest
                  
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}

run msrequest (0,
               ?, /* reqstat ? for all */
               0,
               0,
               0,
               "").
