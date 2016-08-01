def var llTest AS LOG.
llTest = TRUE.

FIND FIRST mobile._File WHERE _File-Name = 'SubSer'.
 
FOR EACH mobile._File-Trig WHERE
         mobile._File-Trig._File-Recid = RECID(mobile._File):
    DISP mobile._File-Trig.
    IF NOT llTest THEN delete mobile._File-Trig. 
END.

FIND FIRST mobile._File WHERE _File-Name = 'DCCLi'.
 
FOR EACH mobile._File-Trig WHERE
         mobile._File-Trig._File-Recid = RECID(mobile._File):
    DISP mobile._File-Trig.
    IF NOT llTest THEN delete mobile._File-Trig. 
END.
