from gearbox.migrations import Migration

class AddFinBank(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('FinBank', area='Sta_Data_256',
                       dump_name='finbank',
                       desc='Parameter data for the bank interfaces (eTupas, eMaksu)')
        t.column('bankname', 'character', format='x(11)', initial='',
                 help='Name of the bank company')
        t.column('interface', 'character', format='x(6)', initial='',
                 help='etupas or emaksu')
        t.column('paramname', 'character', format='x(9)', initial='',
                 help='Name of the parameter')
        t.column('paramvalue', 'character', format='x(20)', initial='',
                 help='Value of the parameter')
        t.column('postform', 'logical', initial='yes',
                 help='Whether the entry must be a A01Y_ form-value')
        t.column('macposition', 'integer', initial='-1',
                 help='Position of this value in the premac-string')
        t.index('bank', ['bankname', 'interface', 'paramname'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('FinBank')

