from gearbox.migrations import Migration

class AddCGMember(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CGMember', area='Sta_Data_256',
                       label='Customer Group Members',
                       dump_name='cgmember',
                       desc='Customer Group Member records;  join customers&cust.groups')
        t.column('CustGroup', 'character', format='x(10)', initial='',
                 label='Customer Group',
                 column_label='Customer Group',
                 help='Individual Code for a Customer Group')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('CustName', 'character', format='x(30)', initial='',
                 label='Customer\'s name',
                 column_label='Customer\'s name')
        t.column('Memo', 'character', format='x(10)', initial='',
                 label='Info',
                 column_label='Info',
                 help='Additional coded information about membership')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CustGroup', ['Brand', 'CustGroup', 'CustNum'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CustName', ['Brand', 'CustGroup', 'CustName', 'CustNum'], area='Sta_Index_2',
                unique=True)
        t.index('CustNum', ['Brand', 'CustNum', 'CustGroup'], area='Sta_Index_2',
                unique=True)
        t.index('CustNum_s', ['CustNum', 'CustGroup'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('CGMember')

