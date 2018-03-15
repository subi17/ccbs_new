Syst.TMSRelation:mAddKeyType("DiscountPlan","Compatibility").

/* ROW 1*/
Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS100_6",
                              "DISCFH300P",
                              "ParentValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS100_6",
                              "DISCFH300P_PRO",
                              "ParentValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS100_6",
                              "CONVDISC_CS20F12",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS100_6",
                              "LRDISC2",
                              "ChildValue",
                              YES).


Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS100_6",
                              "CONVDISC20_3",
                              "ParentValue",
                              YES).

/* ROW2 */
Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "DISCFH300P",
                              "CONVDISC_CS100_6",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "DISCFH300P_PRO",
                              "CONVDISC_CS100_6",
                              "ChildValue",
                              YES).

/* ROW3 */
Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS20F12",
                              "CONVDISC_CS100_6",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS20F12",
                              "LRDISC2",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS20F12",
                              "CONVDISC20_3",
                              "ParentValue",
                              YES).

/* ROW4 */
Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "LRDISC2",
                              "CONVDISC_CS100_6",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "LRDISC2",
                              "CONVDISC_CS20F12",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "LRDISC2",
                              "CONVDISC20_3",
                              "ChildValue",
                              YES).

/*ROW5*/
Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC20_3",
                              "CONVDISC_CS100_6",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC20_3",
                              "CONVDISC_CS20F12",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC20_3",
                              "LRDISC2",
                              "ChildValue",
                              YES).

/* CONVDISC_CS20M12 */

/* Have */
Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS20M12",
                              "CONVDISC_CS100_6",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS20M12",
                              "LRDISC2",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS20M12",
                              "CONVDISC20_3",
                              "ParentValue",
                              YES).

/* Get */
Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC_CS100_6",
                              "CONVDISC_CS20M12",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "LRDISC2",
                              "CONVDISC_CS20M12",
                              "ChildValue",
                              YES).

Syst.TMSRelation:mAddRelation("DiscountPlan",
                              "Compatibility",
                              "CONVDISC20_3",
                              "CONVDISC_CS20M12",
                              "ChildValue",
                              YES).

DEFINE VARIABLE gcMXItemToRemove AS CHARACTER NO-UNDO.
DEFINE VARIABLE gii AS INTEGER NO-UNDO.

gcMXItemToRemove = "LRDISC2,CONVDISC20_3".

DO TRANSACTION gii = 1 TO NUM-ENTRIES(gcMXItemToRemove):

   FIND FIRST MXItem EXCLUSIVE-LOCK WHERE
      MXItem.MXSeq = 158 AND
      MXItem.MXValue = ENTRY(gii,gcMXItemToRemove)
   NO-ERROR.

   IF AVAILABLE MXItem
   THEN DELETE MXItem.

END.