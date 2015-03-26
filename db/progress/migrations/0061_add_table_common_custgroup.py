from gearbox.migrations import Migration

class AddCustGroup(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustGroup', area='Sta_Data_256',
                       label='Customer Groups',
                       dump_name='custgrou',
                       desc='Customer Groups')
        t.column('CustGroup', 'character', format='x(10)', initial='',
                 label='GroupCode',
                 column_label='GroupCode',
                 help='Individual Code for a Customer Group')
        t.column('CGName', 'character', format='x(40)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Group name')
        t.column('CreDate', 'date', format='99-99-99', initial='today',
                 label='Created',
                 column_label='Created',
                 help='Date when group was created')
        t.column('ChgDate', 'date', format='99-99-99',
                 label='Changed',
                 column_label='Changed',
                 help='Date when Group and/or its members were changed')
        t.column('CreUser', 'character', initial='',
                 label='Created by',
                 column_label='Created by',
                 help='User who created this group')
        t.column('ChgUser', 'character', initial='',
                 label='Changed by',
                 column_label='Changed by',
                 help='User who changed/updated this group latest')
        t.column('Memo', 'character', extent=15, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('PrevCrit', 'character', format='x(70)', initial='',
                 label='Criteria',
                 column_label='Criteria',
                 help='Previous Criteria')
        t.column('EnterTask', 'character', format='x(30)', initial='',
                 label='Entering Task',
                 column_label='Enter',
                 help='Task that is performed when customer is entered into group')
        t.column('LeaveTask', 'character', format='x(30)', initial='',
                 label='Leaving Task',
                 column_label='Leave',
                 help='Task that is performed when customer is removed from group')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CGName', ['Brand', 'CGName'], area='Sta_Index_2')
        t.index('CustGroup', ['Brand', 'CustGroup'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CustGroup')

