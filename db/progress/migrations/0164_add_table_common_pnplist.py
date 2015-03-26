from gearbox.migrations import Migration

class AddPNPList(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PNPList', area='Sta_Data_64',
                       dump_name='pnplist')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='CustNum')
        t.column('BDestFrom', 'character', format='x(12)', initial='',
                 column_label='BDestFrom',
                 help='FROM destination for PNP series')
        t.column('PNPSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='PNP Sequence')
        t.column('BDestTo', 'character', format='x(12)', initial='',
                 column_label='BDestTo',
                 help='TO destination for PNP series')
        t.column('PNPGroup', 'character', format='x(10)', initial='',
                 label='GroupCode',
                 column_label='GroupCode',
                 help='PNP group code')
        t.column('PriceList', 'character', initial='',
                 label='Price List',
                 column_label='PList',
                 help='Code (identifier) for a Price List')
        t.column('CLI', 'character', format='x(12)', initial='',
                 column_label='CLI',
                 help='CLI number')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('DialTypeUsed', 'logical', extent=15, format='X/', initial='NO',
                 label='Dialling Types Used',
                 column_label='DT',
                 help='Dialling types used')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From Date',
                 column_label='From',
                 help='First effective date')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To Date',
                 column_label='To',
                 help='Last effective date')
        t.column('MSSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='Subscr.')
        t.column('MemberCLI', 'character', format='x(12)', initial='',
                 label='Member',
                 help='Member of the group (CLI)')
        t.index('CustNum', ['Brand', 'CustNum', 'BDestFrom', 'BDestTo'], area='Sta_Index_2',
                primary=True)
        t.index('CustNum_s', ['CustNum', 'BDestFrom', 'BDestTo'], area='Sta_Index_2')
        t.index('MsSeq', ['MSSeq', 'MemberCLI', ('FromDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('PNPSeq', ['Brand', 'PNPSeq', 'BDestFrom', 'BDestTo'], area='Sta_Index_2')

    def down(self):
        self.drop_table('PNPList')

