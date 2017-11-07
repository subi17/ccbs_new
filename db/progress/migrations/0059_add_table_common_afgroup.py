from gearbox.migrations import Migration

class AddTableAFGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('AFGroup', area="Sta_Data_256", dump_name="actiongr")
        t.column('ActionType', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Action Type", column_label="Type", position=2, order=10, help="Action type")
        t.column('Brand', 'character', format="X(8)", initial="", max_width=16, label="Brand", position=3, order=20, help="Brand")
        t.column('CLIType', 'character', format="X(20)", initial="", max_width=40, label="CLI Type", column_label="CLIType", position=4, order=30, help="CLI type")
        t.column('Limit', 'integer', format="->>>>>>9", initial="0", max_width=4, label="Limit", position=5, order=40, help="Limit")
        t.column('TimeUnit', 'integer', format="->>>>>>9", initial="0", max_width=4, label="Time Unit", column_label="Unit", position=6, order=50, help="Time unit")
        t.column('UnitCount', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Unit Count", column_label="Count", position=7, order=60, help="Unit count")
        t.index('ActionType', [['Brand'], ['ActionType'], ['CLIType']], area="Sta_Index_2", primary=True)
        t.index('CLIType', [['Brand'], ['CLIType']], area="Sta_Index_2")

    def down(self):
        self.drop_table('AFGroup')
