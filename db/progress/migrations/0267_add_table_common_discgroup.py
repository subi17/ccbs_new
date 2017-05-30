from gearbox.migrations import Migration

class AddTableDiscGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('DiscGroup', area="Sta_Data_256", label="Discount Groups", dump_name="discgrp", desc='''Groups for different destinations in order to calculate
volume discounts; used in Discount Plans
''')
        t.column('DiscGroup', 'character', format="x(8)", initial="", max_width=16, label="Discount Group", column_label="Discount Group", position=2, order=10, help="Code of Discount Group")
        t.column('DGName', 'character', format="x(30)", initial="", max_width=60, label="Disc Grp Name", column_label="Discount Group Name", position=3, order=20, help="Name of Discount Group")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1220, label="Memo", column_label="Memo", extent=10, position=4, order=30, help="Memo of DiscountGroup")
        t.index('DiscGroup', [['DiscGroup']], area="Sta_Index_2", primary=True, unique=True)
        t.index('DGName', [['DGName'], ['DiscGroup']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('DiscGroup')
