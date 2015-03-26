from gearbox.migrations import Migration

class AddBItemGroup(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BItemGroup', area='Sta_Data_128',
                       label='Product Groups',
                       dump_name='bitemgro',
                       desc='Product Groups')
        t.column('BIGroup', 'character', initial='',
                 label='Bill.Item Group',
                 column_label='BI Group',
                 help='Billing item group code')
        t.column('BIGName', 'character', format='x(30)', initial='',
                 label='BIGroup Name',
                 column_label='BIGName',
                 help='Billing Item group name')
        t.column('xxMemo', 'character', extent=5, format='x(60)', initial='',
                 label='Memo',
                 column_label='Memo',
                 help='Memo for product groups')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('GroupType', 'integer', format='>9', initial='0',
                 label='Group Type',
                 column_label='Type',
                 help='Group type')
        t.column('ReportCode', 'character', format='x(12)', initial='',
                 label='Report Code',
                 column_label='Report',
                 help='Reporting code')
        t.index('BIGName', ['Brand', 'BIGName'], area='Sta_Index_2')
        t.index('BIGroup', ['Brand', 'BIGroup'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('BItemGroup')

