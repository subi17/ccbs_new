    def var lctable  as char   no-undo init "customer".  /* ordercustomer */
    def var lckey    as char   no-undo init "custnum".   /* orderid */
    def var lhSource as handle no-undo.
    def var lhFind   as handle no-undo.
    def var lhField  as handle no-undo.
    def var lhKey    as handle no-undo.
    def var liField  as int    no-undo. 
    def var lcfields as char   no-undo.
    def var i        as int    no-undo.
    def var j        as int    no-undo.

    for first _file no-lock where
              _file-name = lctable,
         each _field of _file no-lock where
              _field._Data-type = "character" and
              _field._extent = 0:
       lcfields = lcfields + (if lcfields > "" then "," else "") + 
                  _field-name.
    end.
    
    message num-entries(lcfields) "char fields in" lctable
    view-as alert-box.
    
    create buffer lhSource for table lctable.
    
    create query lhFind.
    lhFind:set-buffers(lhSource).
    lhFind:query-prepare('FOR EACH ' + lctable + ' NO-LOCK WHERE ' +
                         lctable + '.Brand = "1"').
    lhFind:query-open.

    repeat:
        lhFind:get-next().

        IF lhFind:query-off-end then leave.
        
        i = i + 1.

        do liField = 1 to num-entries(lcfields):

           lhField = lhSource:buffer-field(entry(lifield,lcfields)).
       
           if index(lhfield:buffer-value,chr(10)) > 0 then do trans:
           
              lhsource:find-current(exclusive-lock).

              lhfield:buffer-value = replace(lhfield:buffer-value,
                                             chr(10),
                                             " ").
        
              lhkey = lhsource:buffer-field(lckey).
                
              disp lhkey:buffer-value format "x(10)"
                   lctable + "." + lhField:name format "x(25)" 
                   lhfield:buffer-value format "x(40)".
              pause.

              j = j + 1.
           end.
        end.

        if i mod 1000 = 0 then do:
           pause 0.
           disp i j with 1 down.
        end.
    END.

    lhFind:query-close().
    
    delete object lhFind.
    delete object lhSource. 
 
    disp i j.

