/* ----------------------------------------------------------------------
  MODULE .......: FeeModel.P
  TASK .........: UPDATE Billing Events
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 28-09-99
  CHANGED ......: 04-10-99 jp urights added
                  04.11.99 pt F6 NOT allowed IF products assigned into Event
                  20.05.02/tk Event logging added
                  05.03.03 tk RUN Mc/memo, tokens
                  24.03.03 jp prompt-for not used 
                  05.09.03 aam brand 
                  06.02.04 jp input custnum for memo
  Version ......: M15
  ---------------------------------------------------------------------- */
 
 /* RUN Mc/bevent without fee model group specification */

 RUN Mc/bevent_run(?). 
