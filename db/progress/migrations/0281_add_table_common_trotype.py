from gearbox.migrations import Migration

class AddTableTroType(Migration):

    database = "common"

    def up(self):
        t = self.table('TroType', area="Sta_Data_256", label="Trouble Type", dump_name="trotype", desc="Trouble Type")
        t.column('TTType', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Type", column_label="Type", position=2, order=10, help="Code of trouble type")
        t.column('TTTName', 'character', format="x(30)", initial="", help="Explanation", max_width=60, label="Explanation", column_label="Explanation", position=3, order=20, description="Explanation")
        t.column('Handler', 'character', format="x(8)", initial="", help="Responsible person (handler) for this kind of troubles", max_width=16, label="Responsible Person", column_label="Responsible Person", position=4, order=30, description="Responsible Person")
        t.column('Memo', 'character', format="X(8)", initial="", max_width=16, label="Memo", column_label="Memo", position=5, order=40, help="Memo")
        t.column('TTCat', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Category", column_label="Category", position=6, order=50, help="Code of trouble category")
        t.column('Duration', 'decimal', format=">>9.99", initial="0.00", max_width=15, label="Duration", column_label="Duration", position=7, order=60, help="Default Type Duration")
        t.index('TTCat', [['TTCat'], ['TTType']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Handler', [['Handler']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TroType')
