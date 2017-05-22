from gearbox.migrations import Migration

class AddTablestarSessionActivity(Migration):

    database = "star"

    def up(self):
        t = self.table('starSessionActivity', area="Sta_Data_256", dump_name="starsesa")
        t.column('usercode', 'character', format="X(12)", initial="", max_width=24, label="Usercode", position=2, order=10, help="Usercode")
        t.column('actionDate', 'date', format="99.99.99", initial=self.unknown, max_width=4, label="Action date", position=3, order=30, help="Action date")
        t.column('daycount', 'integer', format=">>>>>9", initial="0", max_width=4, label="Day count", position=4, order=40, help="Action count per day")
        t.column('hourcount', 'integer', format=">>>>>9", initial="0", max_width=336, label="Hour", extent=24, position=5, order=50, help="Action count per hour")
        t.column('clienttype', 'character', format="X(12)", initial="", max_width=24, label="Client Type", position=6, order=20, help="Client type")
        t.index('userCode', [['usercode'], ['clienttype'], ['actionDate']], area="Sta_Index_2", primary=True, unique=True)
        t.index('actionDate', [['actionDate']], area="Sta_Index_2")
        t.index('clientType', [['clienttype'], ['actionDate']], area="Sta_Index_2")

    def down(self):
        self.drop_table('starSessionActivity')
