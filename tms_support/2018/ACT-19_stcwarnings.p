Syst.TMSRelation:mAddKeyType("CLIType","STCWarningMessage").

/* CASE 1 */
Syst.TMSRelation:mAddRelation("CLIType",
                              "STCWarningMessage",
                              "CONT28",
                              "CONT29",
                              "CONT28_TO_CONT29",
                              NO).

/* CASE 2 */
Syst.TMSRelation:mAddRelation("CLIType",
                              "STCWarningMessage",
                              "CONT29",
                              "CONT28",
                              "CONT29_TO_CONT28",
                              NO).

/* CASE 3 */
Syst.TMSRelation:mAddRelation("CLIType",
                              "STCWarningMessage",
                              "CONVERGENT",
                              "MOBILEONLY",
                              "CONVERGENT_TO_MOBILEONLY",
                              YES).

/* CASE 4 */
Syst.TMSRelation:mAddRelation("CLIType",
                              "STCWarningMessage",
                              "CONT28",
                              "MOBILEONLY",
                              "EXTRALINE_TO_MOBILEONLY",
                              YES).

Syst.TMSRelation:mAddRelation("CLIType",
                              "STCWarningMessage",
                              "CONT29",
                              "MOBILEONLY",
                              "EXTRALINE_TO_MOBILEONLY",
                              YES).

/* CASE 5 */
Syst.TMSRelation:mAddRelation("CLIType",
                              "STCWarningMessage",
                              "CONT28",
                              "CONVERGENT",
                              "EXTRALINE_TO_CONVERGENT",
                              YES).

Syst.TMSRelation:mAddRelation("CLIType",
                              "STCWarningMessage",
                              "CONT29",
                              "CONVERGENT",
                              "EXTRALINE_TO_CONVERGENT",
                              YES).