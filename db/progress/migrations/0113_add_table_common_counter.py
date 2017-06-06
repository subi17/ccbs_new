from gearbox.migrations import Migration

class AddTableCounter(Migration):

    database = "common"

    def up(self):
        t = self.table('Counter', area="Sta_Data_2_256", label="Counter", dump_name="counter", desc="Counter")
        t.column('Brand', 'character', format="x(40)", initial="", max_width=80, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brands")
        t.column('HostTable', 'character', format="x(16)", initial="", max_width=32, label="HostTable", column_label="HostTable", position=3, order=20, help="HostTable")
        t.column('KeyValue', 'character', format="x(20)", initial="", max_width=40, label="KeyValue", column_label="KeyValue", position=4, order=30, help="KeyValue")
        t.column('CounterType', 'integer', format="99", initial="0", max_width=4, label="Counter Type", column_label="Type", position=5, order=40, help="Counter type")
        t.column('CounterAmt', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Amount", position=6, order=50)
        t.column('BeginStamp', 'decimal', format="99999999.99999", decimals=2, initial="0", max_width=17, label="Begin Stamp", position=7, order=60)
        t.column('EndStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="End Stamp", column_label="End Stamp", position=8, order=70)
        t.column('CounterSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="CounterSeq", position=9, order=80)
        t.index('CounterSeq', [['CounterSeq']], area="Sta_Index_4", primary=True, unique=True)
        t.index('HostTable', [['Brand'], ['HostTable'], ['KeyValue'], ['CounterType'], ['EndStamp', 'DESC']], area="Sta_Index_4")

    def down(self):
        self.drop_table('Counter')
