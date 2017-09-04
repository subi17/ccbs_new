from gearbox.migrations import Migration

class AddTableMedTrunk(Migration):

    database = "star"

    def up(self):
        t = self.table('MedTrunk', area="Sta_Data_256", label="MedTrunk", dump_name="medtrunk", desc="CDR preprosessing file CDR ranges")
        t.column('Categ', 'character', format="x(1)", initial="", max_width=2, label="Category", column_label="Category", position=2, order=10, help="Category derived for this range in mediation")
        t.column('TrName', 'character', format="x(12)", initial="", max_width=24, label="Name", column_label="Name", position=3, order=20, help="Trunk Group Name")
        t.column('OpCode', 'character', format="x(4)", initial="", max_width=8, label="Operator", column_label="Operator", position=4, order=40, help="Operator Code")
        t.column('Ident', 'character', format="x(8)", initial="", max_width=16, label="Ident", column_label="Ident", position=5, order=80, help="Meditation Device Name")
        t.column('Type', 'character', format="x(1)", initial="", max_width=2, label="Type", column_label="Type", position=6, order=50, help="Traffic Type")
        t.column('TrFrom', 'character', format="x(4)", initial="0", max_width=4, label="From", column_label="From", position=7, order=60, help="Trunk Group Range From")
        t.column('TrTo', 'character', format="x(4)", initial="0", max_width=4, label="To", column_label="To", position=8, order=70, help="Trunk Group Range To")
        t.column('ExCode', 'character', format="x(8)", initial="0", max_width=4, label="Exchange Code", column_label="Exchange Code", position=9, order=90, help="Exchange Code")
        t.column('Direction', 'character', format="X(3)", initial="", max_width=6, label="IO Type", column_label="IO Type", position=10, order=100, help="Traffic Direction (I,O,IO)")
        t.index('Ident', [['Ident'], ['TrFrom'], ['TrTo'], ['TrName'], ['Categ']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ExCode', [['ExCode'], ['TrFrom'], ['TrTo']], area="Sta_Index_2", unique=True)
        t.index('TrName', [['TrName'], ['Ident'], ['TrFrom'], ['TrTo'], ['Categ']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('MedTrunk')
