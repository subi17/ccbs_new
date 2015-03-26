DEFINE VARIABLE ttInvType AS INTEGER LABEL "InvType" NO-UNDO.
DEFINE VARIABLE ttInvNum  AS INTEGER FORMAT "zzzzzzz9" LABEL "Invoice" NO-UNDO.

UPDATE ttinvnum ttinvtype.

FOR EACH INVOICE NO-LOCK WHERE
    BRAND = "1" AND
    InvType = ttInvType:

    IF ttInvNum <> 0 THEN
        IF invnum <> ttinvnum THEN NEXT.

    DISPLAY ExtInvID invnum invdate duedate custnum invtype.
END.

