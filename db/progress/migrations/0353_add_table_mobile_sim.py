from gearbox.migrations import Migration

class AddTableSIM(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SIM', area="Sta_Data_64", dump_name="sim", desc="Sim cards")
        t.column('ICC', 'character', format="x(20)", initial="", max_width=40, label="SIM Serial no.", column_label="SIM Serial no. (ICC)", position=2, order=10, help="Serial no. (ICC) of an individual SIM card")
        t.column('BillLevel', 'character', format="x(10)", initial="", max_width=20, label="Level", column_label="Level", position=3, order=80, help="Hierarchical level code of customer's billing structure")
        t.column('Stock', 'character', format="x(8)", initial="", max_width=16, label="Stock", column_label="Stock", position=4, order=90, help="Code of Stock Where This Sim is (or was) located")
        t.column('AcClass', 'character', format="x(8)", initial="", max_width=16, label="ACClass", column_label="AcClass", position=5, order=20, help="Access Control Class")
        t.column('SimType', 'character', format="x(12)", initial="", max_width=24, label="Type", column_label="Type", position=6, order=30, help="Type")
        t.column('ManCode', 'character', format="x(8)", initial="", max_width=16, label="Manufacturer", column_label="Manufacturer", position=7, order=40, help="Manufacturer")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=8, order=50, help="Customer's number IF this SIM is delivered to a customer")
        t.column('SimStat', 'integer', format="z9", initial="0", max_width=4, label="Status", column_label="Status", position=9, order=60, help="SIM Status Code")
        t.column('SimArt', 'character', format="x(12)", initial="", max_width=24, label="SimArt", column_label="SimArt", position=10, order=70, help="Article Code for a SIM type")
        t.column('ISC1', 'character', format="x(4)", initial="", max_width=8, label="ISC1", column_label="ISC1", position=11, order=100, help="Issuer's Secret Code")
        t.column('SimBatch', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Batch Sequence", column_label="Batch Seq.", position=12, order=110, help="Sequence for a Simbatch")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=13, order=120, help="Code Of Brand")
        t.column('DealerStat', 'integer', format="z9", initial="0", max_width=4, label="DealerStatus", column_label="DealerStatus", position=14, order=130, help="SIM Status Code for dealer")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="SubSeq", column_label="SubSeq", position=15, order=140, help="Sequence for a Subscription")
        t.index('simseSta_s', [['ICC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['BillLevel'], ['ICC', 'ABBREVIATED']], area="Sta_Index_2")
        t.index('CustNum_s', [['CustNum'], ['BillLevel'], ['ICC', 'ABBREVIATED']], area="Sta_Index_2", unique=True)
        t.index('ISC1', [['Brand'], ['ISC1'], ['ICC', 'ABBREVIATED']], area="Sta_Index_2")
        t.index('SimArt', [['Brand'], ['SimArt'], ['ICC', 'ABBREVIATED']], area="Sta_Index_2")
        t.index('SimBatch', [['Brand'], ['SimBatch'], ['ICC', 'ABBREVIATED']], area="Sta_Index_2")
        t.index('SimSer', [['Brand'], ['ICC']], area="Sta_Index_2", unique=True)
        t.index('SimStat', [['Brand'], ['Stock'], ['SimStat'], ['ICC']], area="Sta_Index_2")
        t.index('Stock', [['Brand'], ['Stock'], ['ICC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('SIM')
