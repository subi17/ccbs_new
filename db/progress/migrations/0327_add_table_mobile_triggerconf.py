from gearbox.migrations import Migration

class AddTableTriggerConf(Migration):

    database = "mobile"

    def up(self):
        t = self.table('TriggerConf', area="Sta_Data_64", dump_name="triggerconf")
        t.column('TriggerConfID', 'character', format="x(14)", initial="", max_width=28, label="TriggerConf", column_label="TriggerConf", position=2, order=10, help="Trigger Configuration code")
        t.column('TCName', 'character', format="x(20)", initial="", max_width=40, label="TriggerConf Name", column_label="TriggerConf Name", position=3, order=20)
        t.column('Prior', 'integer', format=">9", initial="0", max_width=4, label="Priority", column_label="Priority", position=4, order=30, help="Priority order of Trigger Configuration")
        t.column('ValidFrom', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="ValidFrom", column_label="ValidFrom", position=5, order=40, help="The date FROM which this TriggerConf will be used")
        t.column('ValidTo', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="ValidTo", column_label="ValidTo", position=6, order=50, help="The date TO which this Trigger Configuration will be used")
        t.column('EventRule', 'integer', format="9", initial="0", max_width=4, label="Event rule", column_label="EventRule", position=7, order=60, help="TriggerEvent rule")
        t.index('Prior', [['Prior'], ['ValidTo', 'DESC']], area="Sta_Index_1", primary=True, unique=True)
        t.index('TCName', [['TCName'], ['ValidFrom']], area="Sta_Index_1")
        t.index('TriggerConfID', [['TriggerConfID', 'ABBREVIATED']], area="Sta_Index_1")

    def down(self):
        self.drop_table('TriggerConf')
