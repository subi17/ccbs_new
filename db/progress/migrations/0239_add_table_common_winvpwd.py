from gearbox.migrations import Migration

class AddWInvPwd(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('WInvPwd', area='Sta_Data_256',
                       label='Web Password',
                       dump_name='winvpwd',
                       desc='Password for Web Invoice')
        t.column('CustNum', 'integer', mandatory=True, format='ZZZZZZZZ9', initial='0',
                 label='CustNo',
                 column_label='Cust.no',
                 help='Customer\'s number')
        t.column('PwdSeries', 'integer', format='>>>>9', initial='0',
                 label='Series',
                 column_label='Series',
                 help='Password list serial number')
        t.column('Passwd', 'integer', extent=100, format='99999', initial='0',
                 label='Password',
                 column_label='Password',
                 help='THE Password')
        t.column('ValidFrom', 'date', format='99-99-99',
                 column_label='ValidFrom',
                 help='Valid from date')
        t.column('Used', 'integer', format='>>9', initial='0',
                 column_label='Used',
                 help='How many passwords are used')
        t.column('LoginId', 'character', format='X(16)', initial='',
                 column_label='Login',
                 help='Login id for the customer')
        t.column('CSPasswd', 'character', extent=100, case_sensitive=True, format='X(20)', initial='',
                 label='Password',
                 column_label='Passwd',
                 help='A case-sensitive password')
        t.index('CustNum', ['CustNum', 'PwdSeries', 'Used'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('LoginId', ['LoginId'], area='Sta_Index_2')

    def down(self):
        self.drop_table('WInvPwd')

