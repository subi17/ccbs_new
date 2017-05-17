from gearbox.migrations import Migration

class AddTablerscode(Migration):

    database = "common"

    def up(self):
        t = self.table('rscode', area="Sta_Data_256", label="Reseller Codes", dump_name="rscode", desc="Reseller codes")
        t.column('Reseller', 'integer', format="99", initial="0", max_width=4, label="Code", column_label="Code", position=2, order=10, help="Reseller code")
        t.column('RsName', 'character', format="x(8)", initial="", max_width=16, label="Name", column_label="Name", position=3, order=20, help="Name of reseller type")

    def down(self):
        self.drop_table('rscode')
