from gearbox.migrations import Migration

class AddTopupScheme(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('TopupScheme', area='Sta_Data_64',
                       label='Topup Scheme',
                       dump_name='topupscheme',
                       desc='Topup scheme')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('TopupScheme', 'character', format='x(12)', initial='',
                 label='Topup Scheme',
                 column_label='Scheme ID',
                 help='Topup scheme ID')
        t.column('Description', 'character', format='x(40)', initial='',
                 help='Description of topup scheme')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Date when scheme becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Date when scheme expires')
        t.column('TopupSource', 'character', format='x(12)', initial='',
                 label='Topup Source',
                 column_label='Source',
                 help='Source of topup')
        t.column('PPReqPrefix', 'character', initial='',
                 label='Topup Prefix',
                 column_label='Prefix',
                 help='Topup prefix')
        t.column('VatIncl', 'logical', format='Included/Excluded', initial='no',
                 label='Tax Included',
                 column_label='Tax Incl.',
                 help='Tax included in amount')
        t.index('Source', ['Brand', 'TopupSource', ('ToDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('ToDate', ['Brand', ('ToDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('ToupScheme', ['Brand', 'TopupScheme'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TopupScheme')

