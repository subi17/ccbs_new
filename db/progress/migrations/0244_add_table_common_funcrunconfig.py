from gearbox.migrations import Migration

class AddTableFuncRunConfig(Migration):

    database = "common"

    def up(self):
        t = self.table('FuncRunConfig', area="Sta_Data_128", label="Function configuration", dump_name="FuncRunConfig", desc="Configuration for functions")
        t.column('ConfName', 'character', format="x(12)", initial="", max_width=24, label="Name", position=2, order=10, help="Description for configuration")
        t.column('FRConfigID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Configuration ID", column_label="ID", position=3, order=20, help="Unique ID for configuration")
        t.column('RunQty', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Quantity Of Runs", column_label="Runs", position=5, order=40, help="Quantity of parallel runs")
        t.column('Description', 'character', format="x(40)", initial="", max_width=40, label="Description", position=6, order=50, help="Short description of the run")
        t.column('NotifyMail', 'character', format="x(30)", initial="", max_width=60, label="Notify By Mail", column_label="Mail", position=12, order=110, help="Notify by mail")
        t.column('NotifySMS', 'character', format="x(30)", initial="", max_width=60, label="Notify By SMS", column_label="SMS", position=13, order=120, help="Notify by SMS")
        t.column('RunCommand', 'character', format="x(20)", initial="", max_width=40, label="Run Command", column_label="Command", position=14, order=130, help="Command for starting the session")
        t.column('StatusInterval', 'integer', format=">>>>>9", initial="0", max_width=4, label="Status Update Interval", column_label="Status Update", position=15, order=140, help="Interval (quantity of events) for updating status info")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=17, order=160, help="Code of brand")
        t.column('Active', 'logical', format="Yes/No", initial="no", max_width=1, label="Active", position=18, order=170, help="Is configuration active")
        t.index('FRConfigID', [['FRConfigID']], area="Sta_Index_1", primary=True, unique=True)
        t.index('ConfName', [['Brand'], ['ConfName']], area="Sta_Index_1")

    def down(self):
        self.drop_table('FuncRunConfig')
