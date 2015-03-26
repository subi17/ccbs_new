
/* define the ttable used to construct  dialog.p */

DEFINE TEMP-TABLE ttable FIELD ValueId AS CHAR LABEL "Value"
                         FIELD Description AS CHAR LABEL "Description"
                         INDEX idxValueId ValueId.


