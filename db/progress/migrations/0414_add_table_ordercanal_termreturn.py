from gearbox.migrations import Migration

class AddTableTermReturn(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('TermReturn', area="Sta_Data_32", label="Returned terminals", table_trigger=[{'crc': '?', 'procedure': 'rd-termreturn.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-termreturn.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="TermReturn")
        t.column('IMEI', 'character', format="X(12)", initial="", max_width=24, label="IMEI", column_label="IMEI", position=2, order=10)
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=3, order=20)
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Terminal Billing Code", column_label="BillCode", position=4, order=30)
        t.column('DeviceStart', 'logical', format="Yes/No", initial="no", max_width=1, label="Device Start", column_label="DeviceStart", position=5, order=40)
        t.column('DeviceScreen', 'logical', format="Yes/No", initial="no", max_width=1, label="Device Screen", column_label="DeviceScreen", position=6, order=50)
        t.column('ReturnTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Return Time Stamp", column_label="ReturnTS", position=8, order=70)
        t.column('MSISDN', 'character', format="x(12)", initial="", max_width=24, label="MSISDN", column_label="MSISDN", position=9, order=80)
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=10, order=90)
        t.column('TerminalType', 'character', format="x(8)", initial="", max_width=16, label="Terminal Type", column_label="TerminalType", position=11, order=100)
        t.column('EnvelopeNumber', 'character', format="x(12)", initial="", max_width=24, label="Envelope Number", column_label="EnvelopeNumber", position=12, order=110)
        t.column('ReturnChannel', 'character', format="x(16)", initial="", max_width=32, label="RetChannel", column_label="RetChannel", position=13, order=130, help="Return Channel")
        t.column('ContractID', 'character', format="x(12)", initial="", max_width=24, label="Contract ID", column_label="ContractId", position=14, order=120, help="Contract ID")
        t.index('OrderId', [['OrderId'], ['ReturnTS', 'DESC']], area="Sta_Index_1", primary=True, unique=True)
        t.index('ReturnTS', [['ReturnTS', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('TermReturn')
