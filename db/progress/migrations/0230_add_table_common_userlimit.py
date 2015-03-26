from gearbox.migrations import Migration

class AddUserLimit(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('UserLimit', area='Sta_Data_64',
                       label='UserLimit',
                       dump_name='userlimit',
                       desc='User or usergroup limits')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brands')
        t.column('LimitType', 'integer', format='>9', initial='0',
                 column_label='LimitType',
                 help='Limit type')
        t.column('LimitTarget', 'character', format='x(12)', initial='',
                 column_label='LimitTarget',
                 help='Limit target type')
        t.column('LimitTargetID', 'character', format='x(12)', initial='',
                 column_label='LimitTargetID',
                 help='Limit target ID')
        t.column('LimitAmt', 'decimal', decimals=2, format='>>>>>>>>>9.99', initial='0',
                 column_label='LimitAmt',
                 help='Limit amount')
        t.index('Limit', ['Brand', 'LimitType', 'LimitTarget', 'LimitTargetID'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('UserLimit')

