from gearbox.migrations import Migration

class AddTableTriggerField(Migration):

    database = "mobile"

    def up(self):
        t = self.table('TriggerField', area="Sta_Data_64", dump_name="triggerField")
        t.column('FieldName', 'character', format="x(14)", initial="", max_width=28, label="TriggerConf", column_label="TriggerConf", position=2, order=16, help="Trigger Configuration code")
        t.column('TableName', 'character', format="x(14)", initial="", max_width=28, label="TriggerConf", column_label="TriggerConf", position=3, order=13, help="Trigger Configuration code")
        t.column('TriggerConfID', 'character', format="x(14)", initial="", max_width=28, label="TriggerConf", column_label="TriggerConf", position=4, order=10, help="Trigger Configuration code")
        t.index('TriggerConfID', [['TriggerConfID']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('TriggerField')
