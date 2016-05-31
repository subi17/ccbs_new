from gearbox.migrations import Migration

class AddTableMNPProcess(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPProcess', area="Sta_Data_64", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-mnpprocess.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-mnpprocess.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="mnpproce")
        t.column('OrderId', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=2, order=10)
        t.column('FormRequest', 'character', format="x(12)", initial="", max_width=16, position=3, order=20)
        t.column('PortRequest', 'character', format="x(24)", initial="", max_width=16, position=4, order=30)
        t.column('StatusCode', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=5, order=40)
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, position=6, order=50)
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, position=8, order=70)
        t.column('MNPSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, position=9, order=80)
        t.column('UpdateTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, position=10, order=90)
        t.column('MNPType', 'integer', format=">9", initial="0", max_width=4, label="MNPType", column_label="MNPType", position=11, order=100, help="MNP process type")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=12, order=110, help="Code Of Brand")
        t.column('StateFlag', 'integer', format=">9", initial="0", max_width=4, label="StateFlag", column_label="StateFlag", position=13, order=120, help="Additional process state info")
        t.column('StatusReason', 'character', format="x(12)", initial="", max_width=24, label="StatusReason", column_label="StatusReason", position=14, order=141)
        t.column('MNPUpdateTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="MNPUpdateTS", column_label="MNPUpdateTS", position=15, order=130)
        t.column('OperCode', 'character', format="x(3)", initial="", max_width=6, label="OperCode", column_label="OperCode", position=16, order=150, help="Operator code (mnp in = donor, mnp out = receptor)")
        t.column('PortingTime', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="PortingTime", column_label="PortingTime", position=17, order=140)
        t.index('MNPSeq', [['MNPSeq']], area="Sta_Index_2", primary=True)
        t.index('FormRequest', [['FormRequest']], area="Sta_Index_2")
        t.index('MNPType', [['Brand'], ['MNPType'], ['StatusCode'], ['CreatedTS', 'DESC']], area="Sta_Index_2")
        t.index('OperCreated', [['Brand'], ['MNPType'], ['OperCode'], ['CreatedTS', 'DESC']], area="Sta_Index_2")
        t.index('OperPorting', [['Brand'], ['MNPType'], ['OperCode'], ['PortingTime', 'DESC']], area="Sta_Index_2")
        t.index('OperStatus', [['Brand'], ['MNPType'], ['OperCode'], ['StatusCode']], area="Sta_Index_2")
        t.index('OrderId', [['OrderId']], area="Sta_Index_2")
        t.index('PortingTime', [['Brand'], ['MNPType'], ['StatusCode'], ['PortingTime', 'DESC']], area="Sta_Index_2")
        t.index('PortRequest', [['PortRequest']], area="Sta_Index_2")
        t.index('StateFlag', [['Brand'], ['MNPType'], ['StateFlag'], ['StatusCode']], area="Sta_Index_2")
        t.index('StatusCode', [['StatusCode']], area="Sta_Index_2")
        t.index('UpdateTS', [['Brand'], ['MNPType'], ['UpdateTS', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MNPProcess')
