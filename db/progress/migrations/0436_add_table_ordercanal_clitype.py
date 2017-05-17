from gearbox.migrations import Migration

class AddTableCLIType(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('CLIType', area="Sta_Data_256", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-clitype.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-clitype.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="mobtype", desc='''


''')
        t.column('Clitype', 'character', format="x(8)", initial="", max_width=16, label="CLI Type", column_label="CLIType", position=2, order=10, help="CLI Type")
        t.column('CliName', 'character', format="x(25)", initial="", max_width=50, label="CLI Type Name", column_label="Name", position=3, order=20, help="Name of CLI type")
        t.column('BillTarget', 'integer', format=">9", initial="0", max_width=4, label="Number", column_label="Number", position=4, order=30, help="Customer's billing target")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1830, label="Memo", column_label="Memo", extent=15, position=5, order=40, help="Memo text")
        t.column('PricePlan', 'character', format="x(12)", initial="", max_width=24, label="PricePlan Code", column_label="PPlan", position=6, order=50, help="Code of Price Plan")
        t.column('DiscPlan', 'character', format="x(12)", initial="", max_width=24, label="DPCode", column_label="DPlan", position=7, order=60, help="Code of a Discount Plan")
        t.column('MinimAmt', 'decimal', format=">,>>9.99", decimals=2, initial="0", max_width=17, label="MinimPrice", column_label="MinimPrice", position=8, order=70, help="Invoice amount (at least)")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Product", column_label="Product", position=9, order=80, help="Product code, max 16 characters")
        t.column('ServicePack', 'character', format="x(8)", initial="", max_width=16, label="ServPack", column_label="ServPack", position=10, order=90, help="Default ServPack")
        t.column('FeeModel1', 'character', format="x(16)", initial="", max_width=32, label="BEvent", column_label="BEvent", position=11, order=100, help="An unique code for a first mobsub")
        t.column('FeeModel2', 'character', format="x(16)", initial="", max_width=32, label="BEvent", column_label="BEvent", position=12, order=110, help="An unique code for a others mobsub")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=13, order=120, help="Code Of Brand")
        t.column('PenaltyFee', 'character', format="x(8)", initial="", max_width=16, label="Penalty Fee", column_label="Penalty", position=14, order=130, help="Fee model for penalty fee")
        t.column('ContrType', 'integer', format=">9", initial="1", max_width=4, label="Contract Type", column_label="ContrType", position=15, order=140, help="Default contract type")
        t.column('CompareFee', 'decimal', format=">>>>9.999", decimals=3, initial="0", max_width=17, label="Comparison Fee", column_label="Comp.Fee", position=16, order=150, help="Comparison fee for penalty fee determination in STC/BTC")
        t.column('WebDisp', 'logical', format="Yes/No", initial="no", max_width=1, label="Display in Web", column_label="Web", position=17, order=160, help="Display CLI type in web")
        t.column('WebInfo', 'character', format="x(60)", initial="", max_width=120, label="Web Info URL", column_label="URL", position=18, order=170, help="URL for web info")
        t.column('ArAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Receivables Account", column_label="Receiv.", position=19, order=180, help="Account no. for Receivables")
        t.column('PerAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Periodizing Account", column_label="Period.", position=20, order=190, help="Account for periodizing")
        t.column('UnbillAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Unbilled Account", column_label="Unbilled", position=21, order=200, help="Account no. for unbilled events (balance sheet)")
        t.column('ServiceClass', 'character', format="x(4)", initial="", max_width=120, label="ServiceClass", column_label="S.Cl", position=22, order=210, help="Service class info")
        t.column('PayType', 'integer', format="9", initial="0", max_width=4, label="Payment Type", column_label="PayType", position=23, order=220, help="Payment type")
        t.column('StatusCode', 'integer', format="9", initial="1", max_width=4, label="Status", column_label="Status", position=24, order=230)
        t.column('WebStatusCode', 'integer', format="9", initial="1", max_width=4, label="Web Status Code", column_label="WebStatus", position=25, order=240)
        t.column('CommercialFee', 'decimal', format=">>>>9.999", decimals=3, initial="0", max_width=17, label="Commercial Fee", column_label="CommercialFee", position=26, order=250)
        t.column('LineType', 'integer', format="9", initial="0", max_width=4, label="Line Type", column_label="LineType", position=27, order=260)
        t.column('FixedLineType', 'integer', format="9", initial="0", max_width=4, label="Fixed Line Type", column_label="FixedLineType", position=28, order=270)
        t.column('UsageType', 'integer', format="9", initial="1", max_width=4, label="Usage Type", column_label="UsageType", position=29, order=280)
        t.column('BundleType', 'logical', format="True/False", initial="False", max_width=1, label="Bundle Based Type", column_label="BundleType", position=30, order=290)
        t.column('BaseBundle', 'character', format="x(12)", initial="", max_width=24, label="BaseBundle", column_label="BaseBundle", position=32, order=300, help="Base voice/data bundle")
        t.column('FixedLineDownload', 'character', format="x(8)", initial="", max_width=16, label="Fixed Line Download", column_label="FixedLineDownload", position=33, order=310, help="Fixed line download speed")
        t.column('FixedLineUpload', 'character', format="x(8)", initial="", max_width=16, label="Fixed Line Upload", column_label="FixedLineUpload", position=34, order=320, help="Fixed line upload speed")
        t.index('Clitype', [['Brand'], ['Clitype']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CliType_s', [['Clitype']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CLIType')
