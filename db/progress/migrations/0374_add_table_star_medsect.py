from gearbox.migrations import Migration

class AddMedSect(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('MedSect', area='Sta_Data_256',
                       label='MedSect',
                       dump_name='medsect',
                       desc='CDR preprosessing file sections')
        t.column('Name', 'character', format='x(12)', initial='',
                 column_label='Name')
        t.column('Num', 'character', format='x(3)', initial='',
                 column_label='Num',
                 help='Number')
        t.column('Type', 'integer', format='z9', initial='0',
                 column_label='Type')
        t.column('Resvd', 'logical', format='Y/N', initial='no',
                 label='Reserverd',
                 column_label='Reserverd',
                 help='Value Yes/No')
        t.column('Pref', 'character', format='x(12)', initial='',
                 label='Prefix',
                 column_label='Prefix',
                 help='Prefix for this subscriber type')
        t.column('Uniq', 'logical', initial='no',
                 label='Unique',
                 column_label='Unique',
                 help='Is this prefix unique (or use all that begin with this) ?')
        t.column('Abs', 'logical', initial='no',
                 label='Abs.type',
                 column_label='ABS',
                 help='Absolute type')
        t.column('APref', 'character', format='x(2)', initial='',
                 label='Abs.pref',
                 column_label='Abs.pref',
                 help='Absolute prefix')
        t.column('NPref', 'logical', initial='no',
                 label='NatPref',
                 column_label='NatPref',
                 help='Check special national prefix (Yes/No)')
        t.index('Name', ['Name', 'Type'], area='Sta_Index_2',
                unique=True)
        t.index('Type', ['Type'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MedSect')

