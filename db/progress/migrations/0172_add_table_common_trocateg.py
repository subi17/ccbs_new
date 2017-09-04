from gearbox.migrations import Migration

class AddTableTroCateg(Migration):

    database = "common"

    def up(self):
        t = self.table('TroCateg', area="Sta_Data_256", label="Trouble Category", dump_name="trocateg", desc="Trouble Category")
        t.column('TTCat', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Trouble Category", column_label="Trouble Category", position=2, order=10, help="Trouble Category code")
        t.column('Memo', 'character', format="x(30)", initial="", help="Brief description for the category defined", max_width=60, label="Explanation", column_label="Explanation", position=3, order=20, description="Explanation")
        t.column('Handler', 'character', format="x(8)", initial="", help="Responsible person (handler) for this category of troubles", max_width=16, label="Responsible Person", column_label="Responsible Person", position=4, order=30, description="Responsible Person")
        t.column('FgColor', 'integer', format=">>9", initial="0", max_width=4, label="ForeGround Color", column_label="FG Color", position=5, order=40, help="ForeGround Color")
        t.column('BgColor', 'integer', format=">>9", initial="0", max_width=4, label="BackGround Color", column_label="BG Color", position=6, order=50, help="BackGround Color")
        t.column('Duration', 'decimal', format=">>9.99", initial="0.00", max_width=15, label="Duration", column_label="Duration", position=7, order=60, help="Default Category Duration")
        t.index('TTCat', [['TTCat']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Handler', [['Handler']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TroCateg')
