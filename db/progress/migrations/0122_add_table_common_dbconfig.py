from gearbox.migrations import Migration

class AddTableDBConfig(Migration):

    database = "common"

    def up(self):
        t = self.table('DBConfig', area="Sta_Data_128", label="DB Configuration", dump_name="dbconfig", desc="DB configuration")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of Brand")
        t.column('DBConfigID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="DB Config ID", column_label="ID", position=3, order=20, help="Unique ID for configuration")
        t.column('Description', 'character', format="x(50)", initial="", max_width=100, label="Description", position=4, order=30, help="Description")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="From", position=5, order=40, help="Valid from")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="Valid To", column_label="To", position=6, order=50, help="Valid to")
        t.column('TableName', 'character', mandatory=True, format="X(15)", initial="", max_width=30, label="Table Name", column_label="Table", position=7, order=60, help="Table name")
        t.column('Host', 'character', format="x(20)", initial="", max_width=40, label="Host", position=8, order=70, help="Host")
        t.column('Service', 'character', format="x(20)", initial="", max_width=40, label="Service", position=9, order=80, help="Service (port)")
        t.column('DBConnName', 'character', format="x(20)", initial="", max_width=40, label="DB Name", column_label="DB", position=10, order=90, help="Connection name of DB")
        t.column('DBState', 'integer', format="9", initial="0", max_width=4, label="Status", position=11, order=100, help="Status of DB")
        t.column('DirectConnect', 'character', format="x(40)", initial="", max_width=80, label="Direct Connection", column_label="Direct", position=12, order=110, help="Path for direct connection")
        t.column('LogicalName', 'character', format="x(20)", initial="", max_width=40, label="Logical Name", column_label="Logical", position=13, order=120, help="Logical name for DB")
        t.index('DBConfigID', [['DBConfigID']], area="Sta_Index_2", primary=True, unique=True)
        t.index('TableName', [['Brand'], ['TableName'], ['DBState'], ['ToDate', 'DESC']], area="Sta_Index_2")
        t.index('ToDate', [['Brand'], ['ToDate', 'DESC'], ['TableName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('DBConfig')
