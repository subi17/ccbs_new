from gearbox.migrations import Migration

class AddTableMedDefTrunk(Migration):

    database = "star"

    def up(self):
        t = self.table('MedDefTrunk', area="Sta_Data_256", label="MedDefTrunk", dump_name="meddeftr", desc="CDR preprosessing file default CGR values")
        t.column('Categ', 'character', format="x(1)", initial="", max_width=2, label="Category", column_label="Category", position=2, order=10, help="Category")
        t.column('DefName', 'character', format="x(12)", initial="", max_width=24, label="Name", column_label="Name", position=3, order=20, help="Name")
        t.column('Range', 'character', format="x(9)", initial="", max_width=18, label="Range", column_label="Range", position=4, order=30, help="Range")
        t.column('OpCode', 'character', format="x(4)", initial="", max_width=8, label="Operator", column_label="Operator", position=5, order=40, help="Operator")
        t.column('Type', 'character', format="x(1)", initial="", max_width=2, label="Type", column_label="Type", position=6, order=50, help="Type")
        t.column('DefFrom', 'character', format="x(4)", initial="0", max_width=4, label="From", column_label="From", position=7, order=60, help="From")
        t.column('DefTo', 'character', format="x(4)", initial="0", max_width=4, label="To", column_label="To", position=8, order=70, help="To")
        t.column('Ident', 'character', format="x(8)", initial="", max_width=16, label="Ident", column_label="Ident", position=9, order=80, help="Identification")
        t.column('ExCode', 'character', format="x(8)", initial="0", max_width=4, label="Ex-num", column_label="Ex-num", position=10, order=90, help="Exchange number")
        t.index('Ident', [['Ident'], ['DefFrom'], ['DefTo'], ['DefName'], ['Categ']], area="Sta_Index_2", primary=True, unique=True)
        t.index('DefName', [['DefName'], ['Ident'], ['DefFrom'], ['DefTo'], ['Categ']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('MedDefTrunk')
