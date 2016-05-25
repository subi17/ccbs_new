from gearbox.migrations import Migration

class AddTableDPConf(Migration):

    database = "common"

    def up(self):
        t = self.table('DPConf', area="Sta_Data_256", label="Volume Discount Config.", dump_name="dpconf", desc='''Configuration for volume discount. Additional configurations in DpBasis.
''')
        t.column('DiscType', 'integer', format=">9", initial="0", help="Type of the discount", max_width=4, label="Discount Type", column_label="Type", position=4, order=50, description="Fixed(0), based on duration(1) or value(2) or qty(3) of calls")
        t.column('Limit', 'decimal', format=">>>>>>>9.99", decimals=2, initial="0", max_width=240, label="Min. Limit", column_label="Limit", extent=10, position=5, order=60, help="Minimum limit for granting the discount")
        t.column('DiscPrcnt', 'decimal', format=">9.99", decimals=2, initial="0", max_width=120, label="Discount Percent", column_label="Disc.", extent=10, position=6, order=70, help="Volume discount percent")
        t.column('ValidFrom', 'date', format="99-99-99", max_width=4, label="Begin Date", column_label="Begin Date", position=7, order=30, help="Date when discount becomes effective")
        t.column('DPConfNum', 'integer', format="->>>>>>9", initial="0", max_width=4, label="Sequence", column_label="Seq", position=8, order=20, help="Unique sequence nbr for linking")
        t.column('DiscPlan', 'character', format="x(12)", initial="", max_width=24, label="Discount Plan", column_label="DiscPlan", position=9, order=10, help="Code for Discount Plan")
        t.column('ValidTo', 'date', format="99-99-99", max_width=4, label="End Date", column_label="EndDate", position=10, order=41, help="Date when discount becomes invalid")
        t.column('DPCName', 'character', format="x(30)", initial="", max_width=60, label="Description", column_label="Description", position=11, order=40, help="Description of Discount Group")
        t.column('StartFee', 'logical', format="Yes/No", initial="no", max_width=1, label="Starting Charge", column_label="StartChrg", position=13, order=90, help="Is discount calculated from starting charges")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=14, order=100, help="Code Of Brand")
        t.column('StepUsage', 'integer', format="9", initial="0", max_width=4, label="Step Usage", column_label="Steps", position=15, order=110, help="0=Highest %, 1=Each step% (exceeded), 2=Each step% (reached)")
        t.index('DpConfNum', [['DPConfNum']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Basis', [['Brand'], ['DiscPlan'], ['DiscType'], ['ValidFrom', 'DESC']], area="Sta_Index_2")
        t.index('DPCName', [['Brand'], ['DPCName'], ['ValidFrom', 'DESC']], area="Sta_Index_2")
        t.index('ValidFrom', [['Brand'], ['DiscPlan'], ['ValidFrom', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('DPConf')
