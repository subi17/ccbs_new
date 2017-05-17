from gearbox.migrations import Migration

class AddTableRatePlan(Migration):

    database = "common"

    def up(self):
        t = self.table('RatePlan', area="Sta_Data_128", label="Rating Plan", dump_name="rateplan", desc="Rating plan for invoice target")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=100, help="Code Of Brand")
        t.column('RatePlan', 'character', format="x(12)", initial="", max_width=24, label="Rating Plan", column_label="RatePlan", position=7, order=60, help="Rating plan code")
        t.column('RPName', 'character', format="x(30)", initial="", max_width=60, label="Name", position=8, order=70, help="Name of rating plan")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=120, label="Memo", position=9, order=80, help="Memo text")
        t.column('PNPRatePlan', 'character', format="x(12)", initial="", max_width=24, label="PNP Rating", column_label="PNP", position=10, order=90, help="Rating plan for PNP")
        t.index('RatePlan', [['Brand'], ['RatePlan']], area="Sta_Index_2", primary=True, unique=True)
        t.index('RPName', [['Brand'], ['RPName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('RatePlan')
