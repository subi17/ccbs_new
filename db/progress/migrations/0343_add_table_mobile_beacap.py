from gearbox.migrations import Migration

class AddTableBeaCap(Migration):

    database = "mobile"

    def up(self):
        t = self.table('BeaCap', area="Sta_Data_256", dump_name="beacap", desc="Bearer Capability ")
        t.column('BeaCap', 'integer', mandatory=True, valexp='''input bc_number < 65535 and
(input bc_number =  1 or input bc_number > 7)''', format="zzzz9", initial="0", max_width=4, label="BC No.", column_label="BC No.", position=2, order=10, valmsg="Shall be 1 or 8 ... 65534 !", help="Bearer Capability No.")
        t.column('BcName', 'character', format="x(40)", initial="", max_width=80, label="Name", column_label="Name", position=3, order=20, help="Name of Bearer Capability")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=4, order=30, help="Code Of Brand")
        t.index('BeaCap', [['Brand'], ['BeaCap']], area="Sta_Index_3", primary=True, unique=True)
        t.index('BcName', [['Brand'], ['BcName'], ['BeaCap']], area="Sta_Index_3")

    def down(self):
        self.drop_table('BeaCap')
