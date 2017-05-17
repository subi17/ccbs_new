from gearbox.migrations import Migration

class AddTableDiscPlan(Migration):

    database = "common"

    def up(self):
        t = self.table('DiscPlan', area="Sta_Data_128", label="Discount Plan", dump_name="discplan")
        t.column('DiscPlan', 'character', format="x(12)", initial="", max_width=24, label="Discount Plan", column_label="Discount Plan", position=2, order=10, help="Code of a Discount Plan")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1220, label="DiscPlan memo", column_label="DiscPlan memo", extent=10, position=3, order=50, help="Memo of a Discount Plan")
        t.column('DPName', 'character', format="x(40)", initial="", max_width=80, label="DPName", column_label="DPName", position=4, order=40, help="Name of a Discount Plan")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=60, help="Code Of Brand")
        t.index('DiscPlan', [['Brand'], ['DiscPlan']], area="Sta_Index_2", primary=True, unique=True)
        t.index('DPName', [['Brand'], ['DPName'], ['DiscPlan']], area="Sta_Index_2")

    def down(self):
        self.drop_table('DiscPlan')
