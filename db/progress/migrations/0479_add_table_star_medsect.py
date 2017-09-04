from gearbox.migrations import Migration

class AddTableMedSect(Migration):

    database = "star"

    def up(self):
        t = self.table('MedSect', area="Sta_Data_256", label="MedSect", dump_name="medsect", desc="CDR preprosessing file sections")
        t.column('Name', 'character', format="x(12)", initial="", max_width=24, label="Name", column_label="Name", position=2, order=10, help="Name")
        t.column('Num', 'character', format="x(3)", initial="", max_width=6, label="Num", column_label="Num", position=3, order=20, help="Number")
        t.column('Type', 'integer', format="z9", initial="0", max_width=4, label="Type", column_label="Type", position=4, order=30, help="Type")
        t.column('Resvd', 'logical', format="Y/N", initial="no", max_width=1, label="Reserverd", column_label="Reserverd", position=5, order=40, help="Value Yes/No")
        t.column('Pref', 'character', format="x(12)", initial="", max_width=24, label="Prefix", column_label="Prefix", position=6, order=50, help="Prefix for this subscriber type")
        t.column('Uniq', 'logical', format="Yes/No", initial="no", max_width=1, label="Unique", column_label="Unique", position=7, order=60, help="Is this prefix unique (or use all that begin with this) ?")
        t.column('Abs', 'logical', format="Yes/No", initial="no", max_width=1, label="Abs.type", column_label="ABS", position=8, order=70, help="Absolute type")
        t.column('APref', 'character', format="x(2)", initial="", max_width=4, label="Abs.pref", column_label="Abs.pref", position=9, order=80, help="Absolute prefix")
        t.column('NPref', 'logical', format="Yes/No", initial="no", max_width=1, label="NatPref", column_label="NatPref", position=10, order=90, help="Check special national prefix (Yes/No)")
        t.index('Type', [['Type']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Name', [['Name'], ['Type']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('MedSect')
