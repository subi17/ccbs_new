from gearbox.migrations import Migration

class AddTableIMSI(Migration):

    database = "mobile"

    def up(self):
        t = self.table('IMSI', area="Sta_Data_64", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-imsi.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-imsi.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="imsi", desc='''IMSI number
''')
        t.column('IMSI', 'character', format="x(18)", initial="", max_width=36, label="IMSI Number", column_label="IMSI Number", position=2, order=10, help="IMSI Number")
        t.column('ICC', 'character', format="x(24)", initial="", max_width=48, label="Serial no.", column_label="Serial no. (ICC)", position=3, order=20, help="Serial no. (ICC) of an individual SIM card")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=4, order=30, help="Customer's number")
        t.column('PIN1', 'character', format="x(6)", initial="", max_width=12, label="PIN-1", column_label="PIN-1", position=5, order=40, help="PIN Code1")
        t.column('PUK1', 'character', format="x(8)", initial="", max_width=16, label="PUK-1", column_label="PUK-1", position=6, order=50, help="PUK Code 1")
        t.column('PIN2', 'character', format="x(4)", initial="", max_width=8, label="PIN-2", column_label="PIN-2", position=7, order=60, help="PIN Code 2")
        t.column('PUK2', 'character', format="x(8)", initial="", max_width=16, label="PUK-2", column_label="PUK-2", position=8, order=70, help="PUK Code 2")
        t.column('KI', 'character', format="x(32)", initial="", max_width=64, label="Key Identifier", column_label="Key Identifier", position=9, order=80, help="Key Identifier")
        t.column('BillLevel', 'character', format="x(10)", initial="", max_width=20, label="Level", column_label="Level", position=10, order=15, help="Hierarchical level code of Customer's Billing Structure")
        t.column('UserSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="SeqNo", column_label="SeqNo", position=11, order=90, help="Internal, consecutive sequence no of user")
        t.index('IMSI', [['IMSI']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum', [['CustNum'], ['BillLevel'], ['IMSI']], area="Sta_Index_2", unique=True)
        t.index('simser', [['ICC'], ['IMSI']], area="Sta_Index_2", unique=True)
        t.index('UserSeq', [['UserSeq'], ['ICC']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('IMSI')
