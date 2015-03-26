from gearbox.migrations import Migration

class AddDPConf(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('DPConf', area='Sta_Data_256',
                       label='Volume Discount Config.',
                       dump_name='dpconf',
                       desc='Configuration for volume discount. Additional configurations in DpBasis.\
')
        t.column('DiscPlan', 'character', format='x(12)', initial='',
                 label='Discount Plan',
                 column_label='DiscPlan',
                 help='Code for Discount Plan')
        t.column('DPConfNum', 'integer', format='->>>>>>9', initial='0',
                 label='Sequence',
                 column_label='Seq',
                 help='Unique sequence nbr for linking')
        t.column('ValidFrom', 'date', format='99-99-99',
                 label='Begin Date',
                 column_label='Begin Date',
                 help='Date when discount becomes effective')
        t.column('DPCName', 'character', format='x(30)', initial='',
                 label='Description',
                 column_label='Description',
                 help='Description of Discount Group')
        t.column('ValidTo', 'date', format='99-99-99',
                 label='End Date',
                 column_label='EndDate',
                 help='Date when discount becomes invalid')
        t.column('DiscType', 'integer', format='>9', initial='0',
                 label='Discount Type',
                 column_label='Type',
                 help='Type of the discount',
                 description='Fixed(0), based on duration(1) or value(2) or qty(3) of calls')
        t.column('Limit', 'decimal', extent=10, decimals=2, format='>>>>>>>9.99', initial='0',
                 label='Min. Limit',
                 column_label='Limit',
                 help='Minimum limit for granting the discount')
        t.column('DiscPrcnt', 'decimal', extent=10, decimals=2, format='>9.99', initial='0',
                 label='Discount Percent',
                 column_label='Disc.',
                 help='Volume discount percent')
        t.column('StartFee', 'logical', initial='no',
                 label='Starting Charge',
                 column_label='StartChrg',
                 help='Is discount calculated from starting charges')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('StepUsage', 'integer', format='9', initial='0',
                 label='Step Usage',
                 column_label='Steps',
                 help='0=Highest %, 1=Each step% (exceeded), 2=Each step% (reached)')
        t.index('Basis', ['Brand', 'DiscPlan', 'DiscType', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2')
        t.index('DPCName', ['Brand', 'DPCName', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2')
        t.index('DpConfNum', ['DPConfNum'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('ValidFrom', ['Brand', 'DiscPlan', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('DPConf')

