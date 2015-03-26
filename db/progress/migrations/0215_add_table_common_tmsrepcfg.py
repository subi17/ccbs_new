from gearbox.migrations import Migration

class AddTMSRepCfg(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TMSRepCfg', area='Sta_Data_128',
                       label='Report Configuration',
                       dump_name='tmsrepcf',
                       desc='Report configuration')
        t.column('RepName', 'character', initial='',
                 label='Print.name',
                 column_label='Print.name',
                 help='Name of a printout')
        t.column('UserCode', 'character', mandatory=True, initial='',
                 label='User ID',
                 column_label='User ID',
                 help='User ID, 1 - 8 characters')
        t.column('PrinterId', 'character', format='x(24)', initial='',
                 label='Printer',
                 column_label='Printer',
                 help='Logical name for printer')
        t.column('Effect', 'character', format='x(1)', initial='',
                 label='FX',
                 column_label='FX',
                 help='Code of a control code sequence')
        t.column('UpdPerm', 'logical', format='Kyllõ/Ei', initial='yes',
                 label='Ask',
                 column_label='Ask',
                 help='Ask printer\'s setup when starting to print (Y/N)')
        t.column('RepNum', 'integer', format='>>9', initial='0',
                 label='Report nbr',
                 help='Report number')
        t.index('PrinterId', ['PrinterId'], area='Sta_Index_2')
        t.index('RepName', ['RepName', 'UserCode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TMSRepCfg')

