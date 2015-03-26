from gearbox.migrations import Migration

class AddMedTrunk(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('MedTrunk', area='Sta_Data_256',
                       label='MedTrunk',
                       dump_name='medtrunk',
                       desc='CDR preprosessing file CDR ranges')
        t.column('Categ', 'character', format='x(1)', initial='',
                 label='Category',
                 column_label='Category',
                 help='Category derived for this range in mediation')
        t.column('TrName', 'character', format='x(12)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Trunk Group Name')
        t.column('OpCode', 'character', format='x(4)', initial='',
                 label='Operator',
                 column_label='Operator',
                 help='Operator Code')
        t.column('Type', 'character', format='x(1)', initial='',
                 column_label='Type',
                 help='Traffic Type')
        t.column('TrFrom', 'character', format='x(4)', initial='0',
                 label='From',
                 column_label='From',
                 help='Trunk Group Range From')
        t.column('TrTo', 'character', format='x(4)', initial='0',
                 label='To',
                 column_label='To',
                 help='Trunk Group Range To')
        t.column('Ident', 'character', initial='',
                 column_label='Ident',
                 help='Meditation Device Name')
        t.column('ExCode', 'character', initial='0',
                 label='Exchange Code',
                 column_label='Exchange Code')
        t.column('Direction', 'character', format='X(3)', initial='',
                 label='IO Type',
                 column_label='IO Type',
                 help='Traffic Direction (I,O,IO)')
        t.index('ExCode', ['ExCode', 'TrFrom', 'TrTo'], area='Sta_Index_2',
                unique=True)
        t.index('Ident', ['Ident', 'TrFrom', 'TrTo', 'TrName', 'Categ'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('TrName', ['TrName', 'Ident', 'TrFrom', 'TrTo', 'Categ'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('MedTrunk')

