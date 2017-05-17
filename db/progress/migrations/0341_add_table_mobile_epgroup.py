from gearbox.migrations import Migration

class AddTableEPGroup(Migration):

    database = "mobile"

    def up(self):
        t = self.table('EPGroup', area="Sta_Data_256", label="Product group", dump_name="epgrp", desc="External product group")
        t.column('EpGroup', 'character', format="x(12)", initial="", max_width=24, label="EPcode", column_label="EPcode", position=2, order=10, help="Unique code of product group")
        t.column('EpName', 'character', format="x(40)", initial="", max_width=80, label="Name", column_label="Name", position=3, order=20, help="Name of product group")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1220, label="Memo", column_label="Memo", extent=10, position=4, order=30, help="Memo text")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=5, order=40, help="Code Of Brand")
        t.index('EpGroup', [['Brand'], ['EpGroup', 'ABBREVIATED']], area="Sta_Index_3", primary=True, unique=True)
        t.index('EpName', [['Brand'], ['EpName'], ['EpGroup', 'ABBREVIATED']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('EPGroup')
