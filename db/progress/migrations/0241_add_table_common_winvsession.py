from gearbox.migrations import Migration

class AddWInvSession(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('WInvSession', area='Sta_Data_256',
                       label='Web Invoice Session',
                       dump_name='winvsess',
                       desc='Session id for Web Invoice')
        t.column('SessionId', 'character', case_sensitive=True, format='X(50)', initial='',
                 column_label='Id')
        t.column('CustNum', 'integer', format='zzzzzzzz', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='Customer number')
        t.column('LastAction', 'integer', initial='0',
                 label='Last action time',
                 column_label='Action',
                 help='Time stamp for last action made')
        t.column('WsAdd', 'character', initial='',
                 label='Additional')
        t.column('WsAddInt', 'integer', initial='0',
                 label='Additional int')
        t.column('ActionDate', 'date', format='99-99-9999',
                 label='Action Date',
                 column_label='Date',
                 help='Date when last action occurred')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('ActionDate', ['Brand', 'ActionDate'], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum'], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum'], area='Sta_Index_2')
        t.index('SessionId', ['Brand', 'SessionId'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('WInvSession')

