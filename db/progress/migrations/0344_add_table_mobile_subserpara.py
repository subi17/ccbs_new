from gearbox.migrations import Migration

class AddTableSubSerPara(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SubSerPara', area="Sta_Data_128_2", dump_name="subserpa-------1")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="SubSeq", column_label="SubSeq", position=2, order=10, help="Sequence for a Subscription")
        t.column('ServCom', 'character', format="x(12)", initial="", max_width=24, label="Service Component", column_label="Service Component", position=3, order=20, help="Code of Service Component")
        t.column('ParaValue', 'character', format="x(25)", initial="", max_width=50, label="Value", column_label="Value", position=4, order=30, help="Value of Subscription service parameter")
        t.column('ParaName', 'character', format="x(25)", initial="", max_width=50, label="ParaName", column_label="ParaName", position=5, order=40, help="Name of the Subscription parameter name")
        t.column('SSDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Activation Date", column_label="Date", position=6, order=50, help="Date when activated")
        t.column('SologStat', 'integer', format="9", initial="0", help="Solog status of attribute (sent to HLR)", max_width=4, label="Solog Status", column_label="HLR", position=7, order=100, description="0=no need to send, 1=should be sent, 2=sent (solog created)")
        t.column('PDecValue', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Decimal Value", column_label="DecValue", position=8, order=110, help="Decimal parameter value")
        t.index('MSSeq', [['MsSeq'], ['ServCom'], ['ParaName'], ['SSDate', 'DESC']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('SubSerPara')
