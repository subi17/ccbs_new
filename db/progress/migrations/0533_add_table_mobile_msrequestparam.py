from gearbox.migrations import Migration

class AddTableMsRequestParam(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MsRequestParam', area="Sta_Data_64", label="Request Parameter", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-msrequestparam.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-msrequestparam.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="msrequestparam", desc="Request parameter")
        t.column('MsRequest', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Request ID", column_label="ID", position=2, order=10, help="Unique ID for request")
        t.column('ParamName', 'character', format="x(16)", initial="", max_width=32, label="Parameter Name", column_label="Name", position=3, order=20, help="Parameter name")
        t.column('ParamType', 'character', format="x(12)", initial="", max_width=24, label="Parameter Type", column_label="Type", position=4, order=30, help="Parameter type")
        t.column('IntValue', 'integer', format=">>>>>>>>9", initial="0", max_width=21, label="Integer Value", column_label="Integer", position=5, order=40, help="Integer value")
        t.column('DecValue', 'decimal', format="->>>>>9.999", decimals=6, initial="0", max_width=21, label="Decimal Value", column_label="Decimal", position=6, order=50, help="Decimal value")
        t.column('CharValue', 'character', format="X(30)", initial="", max_width=60, label="Character Value", column_label="Character", position=7, order=60, help="Character value")
        t.column('DateValue', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Date Value", column_label="Date", position=8, order=70, help="Date value")
        t.column('ParamSet', 'character', format="x(12)", initial="", max_width=24, label="Parameter Set", column_label="ParamSet", position=9, order=80, help="Parameter Set")
        t.index('ParamName', [['MsRequest'], ['ParamName']], area="Sta_Index_3", primary=True, unique=True)

    def down(self):
        self.drop_table('MsRequestParam')
