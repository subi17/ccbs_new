from gearbox.migrations import Migration

class AddTMSReport(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TMSReport', area='Sta_Data_128',
                       label='Reports',
                       dump_name='tmsrepor',
                       desc='Reports')
        t.column('RepName', 'character', initial='',
                 label='Printname',
                 column_label='Printname',
                 help='Name of a printout')
        t.column('Memo', 'character', format='x(32)', initial='',
                 label='PrtExplanation',
                 column_label='PrtExplanation',
                 help='Name of a printout')
        t.column('PageWidth', 'integer', format='ZZ9', initial='0',
                 label='Width',
                 column_label='Width',
                 help='Maximum number of characters per line on printer')
        t.column('UpdPerm', 'logical', format='Kyllõ/Ei', initial='yes',
                 label='Ask',
                 column_label='Ask',
                 help='Ask printer\'s setup when starting to print (Y/N)')
        t.column('PrintQty', 'integer', format='zzzzz9', initial='0',
                 label='MaxNo',
                 column_label='MaxNo',
                 help='Amount of data when normal user\'s print will be disrupted')
        t.column('EMail', 'character', format='x(40)', initial='',
                 label='E-mail',
                 column_label='E-mail',
                 help='Customer\'s e-mail address')
        t.column('ChEMail', 'logical', initial='no',
                 help='Can user change the eMail address')
        t.index('RepName', ['RepName'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TMSReport')

