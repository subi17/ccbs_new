from gearbox.migrations import Migration

class AddTableIFiSpx(Migration):

    database = "mobile"

    def up(self):
        t = self.table('IFiSpx', area="Sta_Data_256", dump_name="ifispx", desc='''IMSI Delivery File Specifications
''')
        t.column('ManCode', 'character', format="x(8)", initial="", max_width=16, label="Manufact", column_label="Manufact", position=2, order=10, help="Code of Manufacturer")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1220, label="Memo", column_label="Memo", extent=10, position=3, order=40, help="Memo")
        t.column('ICC', 'integer', format="zz9", initial="0", max_width=16, label="ICC", column_label="ICC", extent=2, position=4, order=50, help="Position of ICC Code")
        t.column('Hrowd', 'integer', format="z9", initial="0", max_width=4, label="HRows", column_label="HR", position=5, order=30, help="Number of Header Rows before Data rows")
        t.column('IMSI', 'integer', format="zz9", initial="0", max_width=16, label="IMSI", column_label="IMSI", extent=2, position=6, order=60, help="Position of IMSI Code")
        t.column('IsCode1', 'integer', format="zz9", initial="0", max_width=16, label="PIN1", column_label="PIN1", extent=2, position=7, order=70, help="Position of PIN Code 1")
        t.column('IsCode2', 'integer', format="zz9", initial="0", max_width=16, label="PIN CODE2", column_label="PIN CODE2", extent=2, position=8, order=80, help="Position of PIN Code 2")
        t.column('IsUnb1', 'integer', format="zz9", initial="0", max_width=16, label="UNB1", column_label="UNB1", extent=2, position=9, order=90, help="Position of Unblock Code 1")
        t.column('IsUnb2', 'integer', format="zz9", initial="0", max_width=16, label="UNB2", column_label="UNB2", extent=2, position=10, order=100, help="Position of Unblock Code 2")
        t.column('Version', 'character', format="x(12)", initial="", max_width=24, label="Version", column_label="Version", position=11, order=20, help="Version Number of the file")
        t.column('Ki', 'integer', format="zz9", initial="0", max_width=16, label="Ki", column_label="Ki", extent=2, position=12, order=110, help="Position of Ki Code")
        t.column('SimArt', 'character', format="x(12)", initial="", max_width=24, label="SimArtCode", column_label="SimArtCode", position=13, order=120, help="Article Code for a SIM type")
        t.column('Isc1', 'integer', format="zz9", initial="0", max_width=16, label="ISC1", column_label="ISC1", extent=2, position=14, order=130, help="Position of ISC1 Code")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=15, order=140, help="Code Of Brand")
        t.index('mancode', [['Brand'], ['ManCode'], ['Version']], area="Sta_Index_3", primary=True, unique=True)

    def down(self):
        self.drop_table('IFiSpx')
