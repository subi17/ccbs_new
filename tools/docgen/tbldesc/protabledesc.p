
DEF VAR lcLongDatatypes AS CHAR INITIAL "character,integer,logical,decimal" NO-UNDO.
DEF VAR lcAbbrevDatatypes AS CHAR INITIAL "char,int,log,dec" NO-UNDO.

DEF VAR lcName AS CHAR NO-UNDO.

DEF STREAM lsOut.

OUTPUT STREAM lsOut TO VALUE(ENTRY(1, SESSION:PARAMETER)).
lcName = ENTRY(2, SESSION:PARAMETER).


FIND dictdb._sequence
WHERE dictdb._sequence._seq-name EQ lcName NO-ERROR.
IF AVAILABLE dictdb._sequence THEN DO:
    DEFINE VARIABLE liCurVal AS INT NO-UNDO.
    DEFINE VARIABLE lcTempFile AS CHAR NO-UNDO.
    lcTempFile = SESSION:TEMP-DIRECTORY + "/seq_value.p".
    OUTPUT TO VALUE(lcTempFile).
    PUT UNFORMATTED SUBST("DEFINE OUTPUT PARAMETER result AS INT NO-UNDO.~n").
    PUT UNFORMATTED SUBST("result = CURRENT-VALUE(&1).", lcName).
    OUTPUT CLOSE.
    RUN VALUE(lcTempFile)(OUTPUT liCurVal).
    OS-DELETE VALUE(lcTempFile).
    PUT STREAM lsOut UNFORMATTED "Sequence description for " lcName.
    DISPLAY STREAM lsOut
            dictdb._sequence._seq-name FORMAT "x(18)" LABEL "Name"
            dictdb._sequence._seq-init FORMAT ">>>>>>9" LABEL "Init"
            dictdb._sequence._seq-incr FORMAT ">>>>>9" LABEL "Incr"
            dictdb._sequence._Cycle-Ok LABEL "Cycle"
            dictdb._sequence._seq-min FORMAT ">>>>>>9" LABEL "Min"
            dictdb._sequenc._seq-max  FORMAT ">>>>>>>>>>>>9" LABEL "Max"
            liCurVal FORMAT ">>>>>>>>>>>>9" LABEL "Current".
    PUT STREAM lsOut UNFORMATTED "~n".
END.

FIND dictdb._file
WHERE dictdb._file._file-name EQ lcName NO-ERROR.
IF AVAILABLE dictdb._file THEN DO:
    DEF VAR lcDataType AS CHAR NO-UNDO.
    PUT STREAM lsOut UNFORMATTED "Table description for " lcName.
    FOR EACH dictdb._field OF dictdb._file:
        lcDataType = dictdb._field._data-type.
        IF LOOKUP(lcDataType, lcLongDatatypes) > 0 THEN
            lcDataType = ENTRY(LOOKUP(lcDataType, lcLongDatatypes),
                                                  lcAbbrevDatatypes).
        IF dictdb._field._Extent > 1 THEN
            lcDataType = lcDataType + SUBST("[&1]", dictdb._field._Extent).

        DISPLAY STREAM lsOut
                dictdb._field._field-name FORMAT "x(20)"
                lcDataType FORMAT "x(8)" LABEL "Datatype"
                dictdb._field._initial FORMAT "x(5)"
                dictdb._field._format FORMAT "x(14)"
                dictdb._field._label FORMAT "x(25)".

    END.
    PUT STREAM lsOut UNFORMATTED "~n".
    DEF VAR lcIndexType AS CHAR NO-UNDO.
    DEF VAR lcIndexFields AS CHAR NO-UNDO.
    DEF VAR lcc AS CHAR NO-UNDO.
    PUT STREAM lsOut UNFORMATTED "Index name            Type Index fields" SKIP.
    PUT STREAM lsOut UNFORMATTED "--------------------- ---- --------------------------------------------------" SKIP.
    FOR EACH dictdb._index of dictdb._file
    WHERE dictdb._index._active:
    
        IF dictdb._file._Prime-Index = RECID(dictdb._Index) THEN
            lcIndexType = "P".
        ELSE
            lcIndexType = "".
        IF dictdb._index._unique THEN
            lcIndexType = lcIndexType + " U".
    
        lcIndexFields = "".
        FOR EACH _Index-field OF _Index NO-LOCK,
        _Field OF _Index-field NO-LOCK
        BY _Index-field._Index-seq:
            lcIndexFields = lcIndexFields + ", " + _Field._Field-Name +
                    " (" + TRIM(STRING(_Index-field._Ascending, "ASC/DESC")) + ")".
    
        END.
        lcIndexFields = TRIM(lcIndexFields, ", ").
    
        lcc = "".
        IF LENGTH(lcIndexFields) > 50 THEN DO:
            DEF VAR linew AS INT NO-UNDO.
            DEF VAR lipos AS INT NO-UNDO.
            lipos = 1.
            REPEAT:
                linew = INDEX(lcIndexFields, ",", lipos + 1).
                IF linew EQ 0 OR linew > 50 THEN
                    LEAVE.
                lipos = linew.
            END.
            lcc = STRING("", "x(28)") + SUBSTRING(lcIndexFields, lipos + 1) + "~n".
            lcIndexFields = SUBSTRING(lcIndexFields, 1, lipos).
        END.
    
        PUT STREAM lsOut UNFORMATTED
                STRING(dictdb._index._index-name, "x(21)") + " " +
                STRING(lcIndexType, "x(4)") + " " +
                STRING(lcIndexFields) + "~n".
    
        PUT STREAM lsOut UNFORMATTED lcc.
    END.

END.

OUTPUT STREAM lsOut CLOSE.
QUIT.
