from gearbox.migrations import Migration

class AddTableNatPref(Migration):

    database = "star"

    def up(self):
        t = self.table('NatPref', area="Sta_Data_256", label="NatPref", dump_name="natpref", desc="Special prefixes for national B-sub. numbers")
        t.column('BDest', 'character', format="x(16)", initial="", max_width=32, label="Nat B-sub", column_label="Nat B-sub", position=2, order=10, help="A National B-subscriber number")
        t.column('Pref', 'character', format="x(8)", initial="", max_width=16, label="Prefix", column_label="Prefix", position=3, order=20, help="An alphanumeric prefix that is to be added into B-sub. no")
        t.column('Memo', 'character', format="x(30)", initial="", max_width=60, label="Memo", column_label="Memo", position=4, order=30, help="Memo Text")
        t.index('BDest', [['BDest']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Pref', [['Pref'], ['BDest']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('NatPref')
