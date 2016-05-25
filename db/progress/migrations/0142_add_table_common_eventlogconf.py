from gearbox.migrations import Migration

class AddTableEventLogConf(Migration):

    database = "common"

    def up(self):
        t = self.table('EventLogConf', area="Sta_Data_64", label="Eventlog Configuration", dump_name="eventlogconf", desc='''Eventlog configuration
''')
        t.column('TableName', 'character', mandatory=True, format="X(15)", initial="", max_width=30, label="Table Name", column_label="Table", position=2, order=10, help="Table name")
        t.column('ConfigType', 'character', format="x(12)", initial="", max_width=24, label="Configuration Type", column_label="Type", position=3, order=20, help="Configuration type")
        t.column('ConfigValue', 'character', format="x(30)", initial="", max_width=60, label="Configuration Value", column_label="Value", position=4, order=30, help="Configuration value")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="From", position=5, order=40, help="Valid from")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="Valid To", column_label="To", position=6, order=50, help="Valid to")
        t.index('TableName', [['TableName'], ['ConfigType'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('EventLogConf')
