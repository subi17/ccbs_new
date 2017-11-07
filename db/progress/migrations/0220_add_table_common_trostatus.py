from gearbox.migrations import Migration

class AddTableTroStatus(Migration):

    database = "common"

    def up(self):
        t = self.table('TroStatus', area="Sta_Data_256", label="Trouble Status", dump_name="trostatu", desc="Trouble Status")
        t.column('TTStatus', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Status", column_label="Status", position=2, order=10, help="Code of trouble status")
        t.column('Memo', 'character', format="x(30)", initial="", help="Explanation(description) of trouble status", max_width=60, label="Explanation", column_label="Explanation", position=3, order=20, description="Explanation")
        t.index('TTStatus', [['TTStatus']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('TroStatus')
