DEFINE VARIABLE ttCLIType AS CHAR NO-UNDO.
DEFINE VARIABLE ttActivationDate AS DATE FORMAT "99.99.99" NO-UNDO.

UPDATE
    ttCLIType LABEL "CLIType"
    ttActivationDate LABEL "Since".

FOR EACH MobSub NO-LOCK WHERE
    BRAND = "1"         AND
    CLIType = ttCLIType AND
    ActivationDate >= ttActivationDate:

    DISPLAY
        CLI
        Custnum
        MsSeq LABEL "Subscription id"
        ActivationDate LABEL "Activated".
END.
