from gearbox.migrations import Migration

class AddTableExpLog(Migration):

    database = "common"

    def up(self):
        t = self.table('ExpLog', area="Sta_Data_256", label="Transfer Log", dump_name="explog", desc="Transfer log for exported data ")
        t.column('ExpNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="ID number", column_label="ID nr.", position=2, order=10, help="Identification number for exported data")
        t.column('DataType', 'character', format="x(8)", initial="", max_width=16, label="Type", column_label="Type", position=3, order=20, help="Type of exported data")
        t.column('ExpStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="XStamp", column_label="XStamp", position=4, order=30, help="Time stamp for exported data")
        t.column('ExpType', 'character', format="x(8)", initial="", max_width=16, label="ExpType", column_label="ExpType", position=5, order=40, help="Export type of data, ie. receiver type")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('ExpType', [['Brand'], ['ExpType'], ['DataType'], ['ExpNum']], area="Sta_Index_2", primary=True, unique=True)
        t.index('DataType', [['Brand'], ['DataType'], ['ExpType'], ['ExpNum']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('ExpLog')
