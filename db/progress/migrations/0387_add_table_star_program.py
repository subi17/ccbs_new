from gearbox.migrations import Migration

class Addprogram(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('program', area='Sta_Index_2',
                       label='Program',
                       desc='Program specification')
        t.column('program', 'character', format='X(50)', initial='',
                 label='Program',
                 help='Program with recursize directory')
        t.column('name', 'character', format='X(40)', initial='',
                 label='Name',
                 help='Program name. Use menu labels and program title')
        t.column('programcode', 'character', format='X(12)', initial='',
                 label='Programcode')
        t.column('paramstr', 'character', format='X(40)', initial='',
                 label='Parameters',
                 help='Parameters to program',
                 description='Program got these information in PRIVATE-DATA')
        t.column('iconfile', 'character', format='X(40)', initial='',
                 label='Iconfile',
                 help='Icon shows in menues..')
        t.column('menutooltip', 'character', format='X(256)', initial='',
                 label='Tooltip',
                 help='Program tooltip. Shows on menus...')
        t.column('rightgroup', 'character', format='X(12)', initial='',
                 label='RightGroup',
                 help='Right group code')
        t.column('programstastus', 'logical', initial='yes',
                 label='Active',
                 help='Program is runnable from menu')
        t.column('programtype', 'character', initial='',
                 label='Type',
                 help='Program type')
        t.column('filegroup', 'character', format='X(12)', initial='',
                 label='File Group',
                 help='File group')
        t.column('canCreate', 'logical', initial='no',
                 label='Create',
                 help='Can create')
        t.column('canCopy', 'logical', initial='no',
                 label='Copy',
                 help='Can copy')
        t.column('canDelete', 'logical', initial='no',
                 label='Delete',
                 help='Can Delete')
        t.column('canModified', 'logical', initial='no',
                 label='Modified',
                 help='Can modified')
        t.column('PrintDestinations', 'character', format='X(40)', initial='',
                 label='Print destinations',
                 help='Available print destinations')
        t.column('archive', 'character', format='X(10)', initial='Default',
                 label='Archive',
                 help='Archive time',
                 description='Default\
Year\
Month\
Week\
Permanent')
        t.index('name', ['name'], area='Sta_Index_2')
        t.index('program', ['program'], area='Sta_Index_2')
        t.index('programcode', ['programcode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('program')

